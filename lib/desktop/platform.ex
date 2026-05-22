defmodule Desktop.Platform do
  @moduledoc false

  alias Desktop.{Backend, OS}

  @type capabilities :: %{
          window: boolean(),
          content: :webview | :native | :os_browser,
          notification: :wx | :native | :log,
          menu: :wx | :native | :dbus | :none,
          taskbar: boolean()
        }

  @doc """
  Returns the active backend module implementing Platform behaviours.
  """
  @spec backend() :: module()
  def backend do
    case Application.get_env(:desktop, :backend, :auto) do
      :wx -> Backend.Wx
      :json -> Backend.Json
      :browser -> Backend.Browser
      :auto -> detect_backend()
      mod when is_atom(mod) and mod != :auto -> mod
    end
  end

  @doc """
  Capability map for the active backend.
  """
  @spec capabilities() :: capabilities()
  def capabilities do
    backend().capabilities()
  end

  @doc """
  Returns the webview backend name string stored in Env (wx-specific).
  """
  @spec webview_backend_name() :: String.t()
  def webview_backend_name do
    Desktop.Env.get(:webview_backend, "nil")
  end

  @doc """
  GenServer wrapper used instead of `:wx_object` when wx is not the process driver.
  """
  @spec window_server() :: module()
  def window_server do
    if backend() == Backend.Wx and wx_object_available?() do
      :wx_object
    else
      Desktop.Platform.Server
    end
  end

  defp detect_backend do
    cond do
      mobile_target?() -> Backend.Json
      browser_mode?() -> Backend.Browser
      true -> Backend.Wx
    end
  end

  defp mobile_target? do
    Mix.target() in [:android, :ios] or OS.mobile?()
  end

  defp browser_mode? do
    System.get_env("NO_WX") != nil or not wx_app_available?()
  end

  defp wx_app_available? do
    :application.get_application(:wx) != {:error, :not_loaded} and
      Code.ensure_loaded?(:wx)
  end

  defp wx_object_available? do
    Code.ensure_loaded?(:wx_object) and function_exported?(:wx_object, :start_link, 4)
  end
end
