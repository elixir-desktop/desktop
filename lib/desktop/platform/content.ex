defmodule Desktop.Platform.Content do
  @moduledoc false

  alias Desktop.Platform.Helpers

  @callback attach(frame :: term()) :: term() | nil
  @callback load_url(content :: term() | nil, frame :: term() | nil, url :: String.t() | nil) ::
              :ok
  @callback current_url(content :: term() | nil, last_url :: String.t() | nil) :: String.t() | nil
  @callback content_show(
              content :: term() | nil,
              frame :: term() | nil,
              url :: String.t() | nil,
              only_open :: boolean()
            ) :: :ok
  @callback rebuild(frame :: term() | nil, last_url :: String.t() | nil) :: term() | nil
  @callback reload(content :: term() | nil) :: :ok
  @callback put_webview_backend(name :: String.t()) :: :ok

  def attach(frame), do: Helpers.with_wx_env(fn -> impl().attach(frame) end)

  def load_url(content, frame, url),
    do: Helpers.with_wx_env(fn -> impl().load_url(content, frame, url) end)

  def current_url(content, last_url),
    do: Helpers.with_wx_env(fn -> impl().current_url(content, last_url) end)

  def show(content, frame, url, only_open),
    do: Helpers.with_wx_env(fn -> impl().content_show(content, frame, url, only_open) end)

  def rebuild(frame, last_url),
    do: Helpers.with_wx_env(fn -> impl().rebuild(frame, last_url) end)

  @doc """
  Reloads the webview / native content handle.

  Replaces direct `:wxWebView.reload/1` so the call works on Wx and Json
  (mobile bridge) backends. No-op when content is `nil` or on Browser.
  """
  def reload(content),
    do: Helpers.with_wx_env(fn -> impl().reload(content) end)

  def put_webview_backend(name),
    do: Helpers.with_wx_env(fn -> impl().put_webview_backend(name) end)

  defp impl, do: Desktop.Platform.backend()
end
