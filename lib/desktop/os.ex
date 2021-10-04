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

  @target Mix.target()
  @spec type :: Linux | MacOS | Windows | Android | IOS
  def type() do
    case @target do
      :android ->
        Android

      :ios ->
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
    spawn(fn ->
      Process.sleep(300)

      if windows?() do
        System.stop(0)
      else
        kill_heart()
        System.halt(0)
      end
    end)
  end

  def restart() do
    if windows?() do
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
        System.cmd("kill", ["-9", "#{heart_pid}"], stderr_to_stdout: true)
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
end
