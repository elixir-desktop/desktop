defmodule Desktop.Platform.Content do
  @moduledoc false

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
  @callback put_webview_backend(name :: String.t()) :: :ok

  def attach(frame), do: impl().attach(frame)
  def load_url(content, frame, url), do: impl().load_url(content, frame, url)
  def current_url(content, last_url), do: impl().current_url(content, last_url)

  def show(content, frame, url, only_open),
    do: impl().content_show(content, frame, url, only_open)

  def rebuild(frame, last_url), do: impl().rebuild(frame, last_url)
  def put_webview_backend(name), do: impl().put_webview_backend(name)

  defp impl, do: Desktop.Platform.backend()
end
