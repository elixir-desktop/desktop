defmodule Desktop.Platform.Notification do
  @moduledoc false

  @callback new(title :: String.t(), type :: atom()) :: term() | nil
  @callback notification_show(
              notification :: term() | nil,
              message :: String.t(),
              timeout :: integer(),
              title :: String.t() | nil
            ) :: :ok
  @callback close(notification :: term() | nil) :: :ok

  def new(title, type), do: impl().new(title, type)

  def show(notification, message, timeout, title \\ nil),
    do: impl().notification_show(notification, message, timeout, title)

  def close(notification), do: impl().close(notification)

  defp impl, do: Desktop.Platform.backend()
end
