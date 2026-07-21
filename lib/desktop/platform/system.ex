defmodule Desktop.Platform.System do
  @moduledoc false

  alias Desktop.Platform.Helpers

  @callback init_env() :: {wx :: term(), env :: term()}
  @callback subscribe_events() :: :ok
  @callback set_env(env :: term()) :: :ok
  @callback get_env() :: term()
  @callback locale() :: String.t() | nil
  @callback connect_menu(
              object :: term(),
              command :: atom(),
              callback :: function(),
              id :: term()
            ) ::
              term()
  @callback wx_available?() :: boolean()
  @callback open_external_url(String.t()) :: :ok
  @callback os_description() :: String.t() | charlist() | nil
  @callback activate_event_active?(event :: term()) :: boolean()

  def init_env, do: impl().init_env()
  def subscribe_events, do: impl().subscribe_events()
  def set_env(env), do: impl().set_env(env)
  def get_env, do: impl().get_env()
  def locale, do: Helpers.with_wx_env(fn -> impl().locale() end)

  def connect_menu(object, command, callback, id \\ nil) do
    Helpers.with_wx_env(fn -> impl().connect_menu(object, command, callback, id) end)
  end

  def wx_available?, do: impl().wx_available?()
  def open_external_url(url), do: Helpers.with_wx_env(fn -> impl().open_external_url(url) end)

  @doc """
  Returns a human-readable OS / device description string.

  Replaces direct `:wx_misc.getOsDescription/0` calls so apps work on all
  backends (Wx, Json/mobile bridge, Browser). Returns `nil` when unavailable.
  """
  def os_description do
    raw =
      Helpers.with_wx_env(fn ->
        case impl().os_description() do
          desc when is_binary(desc) -> desc
          desc when is_list(desc) -> List.to_string(desc)
          _ -> nil
        end
      end)

    if raw do
      case String.trim(raw) do
        "" -> nil
        trimmed -> trimmed
      end
    end
  end

  def activate_event_active?(event),
    do: Helpers.with_wx_env(fn -> impl().activate_event_active?(event) end)

  defp impl, do: Desktop.Platform.backend()
end
