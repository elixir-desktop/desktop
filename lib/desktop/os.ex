defmodule Desktop.OS do
  @moduledoc """
    The OS module provides shortcuts and helper functions
    to access OS specific information.

    Most significant one should use OS.type() to differentiate
    between the currently supported environments:
    - Android
    - IOS
    - MacOS
    - Windows
    - Linux

  """

  @doc """
    Returns the users home directory
  """
  @spec home :: binary
  def home() do
    {:ok, [[home]]} = :init.get_argument(:home)
    List.to_string(home)
  end

  @doc false
  def raise_frame(frame) when frame in [nil, :undefined], do: :ok

  def raise_frame(frame) do
    Desktop.Platform.Window.raise_window(frame)
  end

  @spec type :: Linux | MacOS | Windows | Android | IOS
  def type() do
    case System.get_env("ELIXIR_DESKTOP_OS", nil) do
      "android" ->
        Android

      "ios" ->
        IOS

      _ ->
        case :os.type() do
          {:unix, :darwin} -> MacOS
          {:unix, :linux} -> Linux
          {:win32, _} -> Windows
        end
    end
  end

  def shutdown() do
    spawn(&halt/0)
  end

  @spec halt() :: no_return()
  defp halt() do
    Process.sleep(300)

    # With System.stop(0) shutdown of the WebView takes
    # a very long time (10+ seconds), and wxWidgets cleanup
    # often crashes on macOS
    # System.stop(0)

    kill_heart()
    System.halt(0)
  end

  def restart() do
    if windows?() or mobile?() do
      # on windows reinitializing wx-widgets does work sometimes...
      :init.restart()
    else
      # relying on heart
      kill_beam()
    end
  end

  def windows?() do
    type() == Windows
  end

  def mobile?() do
    case type() do
      Android -> true
      IOS -> true
      _other -> false
    end
  end

  defp kill_heart() do
    heart = Process.whereis(:heart)

    if heart != nil do
      {:links, links} = Process.info(Process.whereis(:heart), :links)
      port = Enum.find(links, fn id -> is_port(id) end)

      if port != nil do
        {:os_pid, heart_pid} = Port.info(port, :os_pid)

        kill_process(heart_pid)

        # kill thyself
        kill_beam()
      end
    end
  end

  defp kill_beam() do
    kill_process(:os.getpid())
  end

  def kill_process(pid) do
    if windows?() do
      System.cmd("taskkill", ["/f", "/pid", "#{pid}"], stderr_to_stdout: true)
    else
      System.cmd("kill", ["-9", "#{pid}"], stderr_to_stdout: true)
    end
  end

  @doc """
  This is a Path.expand variant that normalizes the drive letter
  on windows
  """
  @spec path_expand(IO.chardata()) :: binary
  def path_expand(path) do
    if windows?() do
      path = if is_list(path), do: List.to_string(path), else: path
      drv = String.downcase(String.first(path))

      if byte_size(path) == 2 and String.at(path, 1) == ":" do
        Path.expand(path <> "/")
      else
        Path.expand(path)
      end
      |> String.replace_prefix(drv <> ":", String.upcase(drv) <> ":")
    else
      Path.expand(path)
    end
  end

  @doc """
  Opens a URL or file path in the platform default handler.

  Delegates to `Desktop.Platform.System.open_external_url/1` on the active backend
  (Json bridge RPC on mobile, host `open`/`xdg-open`/`cmd` or wx on desktop).
  """
  def launch_default_browser(file) when is_list(file) do
    List.to_string(file)
    |> launch_default_browser()
  end

  def launch_default_browser(file) when is_binary(file) do
    spawn(fn -> Desktop.Platform.System.open_external_url(file) end)
  end

  def open_url(url), do: launch_default_browser(url)

  def open_path(path) do
    fn ->
      case type() do
        MacOS ->
          System.cmd("open", [path], stderr_to_stdout: true, parallelism: true)

        Linux ->
          System.cmd("xdg-open", [path],
            stderr_to_stdout: true,
            env: linux_env(),
            parallelism: true
          )

        Windows ->
          # credo:disable-for-next-line Credo.Check.Warning.UnsafeExec
          :os.cmd(~c"explorer \"#{String.replace(path, "/", "\\")}\"")

        _other ->
          Desktop.OS.launch_default_browser("file://#{path}")
      end
    end
    |> spawn_link()
  end

  def open_file_location(file) do
    fn ->
      case type() do
        MacOS ->
          System.cmd("open", ["-R", file], stderr_to_stdout: true, parallelism: true)

        Linux ->
          case System.find_executable("nautilus") do
            nil ->
              open_path(Path.dirname(file))

            nautilus ->
              System.cmd(nautilus, [file],
                stderr_to_stdout: true,
                env: linux_env(),
                parallelism: true
              )
          end

        Windows ->
          # credo:disable-for-next-line Credo.Check.Warning.UnsafeExec
          :os.cmd(~c"explorer /select,\"#{String.replace(file, "/", "\\")}\"")

        _other ->
          Desktop.OS.launch_default_browser("file://#{file}")
      end
    end
    |> spawn_link()
  end

  defp linux_env() do
    # Unsetting env vars that might interfere with the desktop environment
    ~w(GDK_BACKEND LD_LIBRARY_PATH LD_PRELOAD GIO_MODULE_DIR GDK_PIXBUF_MODULE_FILE GST_PLUGIN_PATH GST_PLUGIN_SYSTEM_PATH GST_REGISTRY)
    |> Enum.map(fn key -> {key, nil} end)
  end
end
