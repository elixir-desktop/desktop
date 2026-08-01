defmodule Desktop.Impl.HostBrowser do
  @moduledoc false
  # Opens a URL or file path with the OS desktop handler (open / xdg-open / cmd).

  alias Desktop.OS

  @spec open(String.t() | charlist()) :: :ok
  def open(url) when is_list(url), do: open(List.to_string(url))

  def open(url) when is_binary(url) do
    spawn(fn -> run_open(url) end)
    :ok
  end

  defp run_open(url) do
    try do
      case OS.type() do
        MacOS ->
          System.cmd("open", [url], stderr_to_stdout: true, parallelism: true)

        Linux ->
          System.cmd("xdg-open", [url],
            stderr_to_stdout: true,
            parallelism: true,
            env: linux_env()
          )

        Windows ->
          System.cmd("cmd", ["/c", "start", "", url], stderr_to_stdout: true, parallelism: true)

        _ ->
          :ok
      end
    catch
      :exit, _ -> :ok
    end

    :ok
  end

  defp linux_env do
    ~w(GDK_BACKEND LD_LIBRARY_PATH LD_PRELOAD GIO_MODULE_DIR GDK_PIXBUF_MODULE_FILE GST_PLUGIN_PATH GST_PLUGIN_SYSTEM_PATH GST_REGISTRY)
    |> Enum.map(fn key -> {key, nil} end)
  end
end
