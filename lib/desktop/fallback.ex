defmodule Desktop.Fallback do
  @moduledoc """
    Fallback handles version differences in the :wx modules needed for showing the
    WebView and Desktop notifications and it uses the highest available
    feature level while trying to stays backwards compatible to older :wx versions.

    Delegates to `Desktop.Platform` backends.
  """

  alias Desktop.Platform

  def webview_new(frame), do: Platform.Content.attach(frame)

  def webview_backend_env, do: System.get_env("WX_WEBVIEW_BACKEND", "none")

  def webview_can_fix(nil), do: false

  def webview_can_fix(webview) do
    Platform.backend() == Desktop.Backend.Wx and
      Desktop.Backend.Null.module?(:wxWebView) and
      Desktop.OS.windows?() and
      webview_backend_available?("wxWebViewEdge") and
      Desktop.Backend.Null.wx_call(:wxWebView, :isShownOnScreen, [webview])
  end

  def webview_url(%Desktop.Window{webview: nil, last_url: last_url}), do: last_url

  def webview_url(%Desktop.Window{webview: webview, last_url: last_url}) do
    Platform.Content.current_url(webview, last_url)
  end

  def webview_load(%Desktop.Window{} = window, url) do
    Platform.Content.load_url(window.webview, window.frame, url)
  end

  def webview_show(%Desktop.Window{} = window, url, only_open) do
    Platform.Content.show(window.webview, window.frame, url, only_open)
  end

  def webview_rebuild(%Desktop.Window{frame: frame, last_url: url}) do
    Platform.Content.rebuild(frame, url)
  end

  def notification_new(title, type), do: Platform.Notification.new(title, type)

  def notification_show(notification, message, timeout, title \\ nil) do
    Platform.Notification.show(notification, message, timeout, title)
  end

  def notification_close(notification), do: Platform.Notification.close(notification)

  def wx_subscribe, do: Platform.System.subscribe_events()

  def wx_new(_opts), do: Platform.System.init_env() |> elem(0)

  def wx_get_env, do: Platform.System.get_env()

  defp webview_backend_available?(backend) do
    try do
      Desktop.Backend.Null.wx_call(:wxWebView, :isBackendAvailable, [String.to_charlist(backend)])
    rescue
      _ -> false
    end
  end
end
