defmodule Desktop.Platform.System do
  @moduledoc false

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

  def init_env, do: impl().init_env()
  def subscribe_events, do: impl().subscribe_events()
  def set_env(env), do: impl().set_env(env)
  def get_env, do: impl().get_env()
  def locale, do: impl().locale()

  def connect_menu(object, command, callback, id \\ nil),
    do: impl().connect_menu(object, command, callback, id)

  def wx_available?, do: impl().wx_available?()

  defp impl, do: Desktop.Platform.backend()
end
