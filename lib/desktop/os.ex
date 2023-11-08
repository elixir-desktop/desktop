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
  def raise_frame(frame) do
    if type() == MacOS do
      name = System.get_env("EMU", "beam.smp")

      fn -> System.cmd("open", ["-a", name], stderr_to_stdout: true) end
      |> spawn_link()
    else
      # Calling  this on wxDirDialog segfaults on macos..
      :wxTopLevelWindow.setFocus(frame)
      :wxWindow.raise(frame)
    end
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

        if windows?() do
          System.cmd("taskkill", ["/f", "/pid", "#{heart_pid}"], stderr_to_stdout: true)
        else
          System.cmd("kill", ["-9", "#{heart_pid}"], stderr_to_stdout: true)
        end

        # kill thyself
        kill_beam()
      end
    end
  end

  defp kill_beam() do
    System.cmd("kill", ["-9", "#{:os.getpid()}"], stderr_to_stdout: true)
  end

  @doc """
  This is a Path.expand variant that normalizes the drive letter
  on windows
  """
  @spec path_expand(binary) :: binary
  def path_expand(path) do
    if windows?() do
      path = Path.expand(path)
      drv = String.first(path)
      String.replace_prefix(path, drv <> ":", String.upcase(drv) <> ":")
    else
      Path.expand(path)
    end
  end

  @doc """
  Replacement for the :wx_misc.launchDefaultBrowser function
  """
  def launch_default_browser(file) when is_list(file) do
    List.to_string(file)
    |> launch_default_browser()
  end

  def launch_default_browser(file) do
    spawn(fn ->
      case type() do
        MacOS ->
          System.cmd("open", [file], stderr_to_stdout: true, parallelism: true)

        Linux ->
          System.cmd("xdg-open", [file], stderr_to_stdout: true, parallelism: true)

        _other ->
          Desktop.Env.wx_use_env()
          :wx_misc.launchDefaultBrowser(file)
      end
    end)
  end
end
