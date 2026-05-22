defmodule Desktop.Platform.Notification do
  @moduledoc false

  alias Desktop.Platform.Helpers

  @callback new(title :: String.t(), type :: atom()) :: term() | nil
  @callback notification_show(
              notification :: term() | nil,
              message :: String.t(),
              timeout :: integer(),
              title :: String.t() | nil
            ) :: :ok
  @callback close(notification :: term() | nil) :: :ok

  def new(title, type), do: Helpers.with_wx_env(fn -> impl().new(title, type) end)

  def show(notification, message, timeout, title \\ nil),
    do:
      Helpers.with_wx_env(fn ->
        impl().notification_show(notification, message, timeout, title)
      end)

  def close(notification), do: Helpers.with_wx_env(fn -> impl().close(notification) end)

  defp impl, do: Desktop.Platform.backend()
end
