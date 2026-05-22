defmodule Desktop.Platform.Window do
  @moduledoc false

  alias Desktop.Platform.Helpers

  @type handle :: term()
  @type content_handle :: term() | nil

  @callback open(keyword()) :: {:ok, handle(), content_handle()} | {:error, term()}
  @callback destroy_frame(handle()) :: :ok
  @callback connect(handle(), event :: atom(), (term() -> :ok)) :: :ok
  @callback show(handle(), keyword()) :: :ok
  @callback hide(handle()) :: :ok
  @callback set_title(handle(), String.t()) :: :ok
  @callback set_min_size(handle(), {integer(), integer()}) :: :ok
  @callback set_icon(handle(), term()) :: :ok
  @callback set_menubar(handle(), term()) :: :ok
  @callback iconize(handle(), boolean()) :: :ok
  @callback shown?(handle()) :: boolean()
  @callback active?(handle()) :: boolean()
  @callback raise_window(handle()) :: :ok
  @callback update_apple_menu(String.t(), handle(), term()) :: :ok
  @callback new_menubar() :: term()
  @callback on_crash_destroy(handle()) :: :ok
  @callback close_event_veto(term()) :: :ok

  def open(opts), do: Helpers.with_wx_env(fn -> impl().open(opts) end)
  def destroy(frame), do: Helpers.with_wx_env(fn -> impl().destroy_frame(frame) end)

  def connect(frame, event, fun),
    do: Helpers.with_wx_env(fn -> impl().connect(frame, event, fun) end)

  def show(frame, opts \\ []), do: Helpers.with_wx_env(fn -> impl().show(frame, opts) end)
  def hide(frame), do: Helpers.with_wx_env(fn -> impl().hide(frame) end)
  def set_title(frame, title), do: Helpers.with_wx_env(fn -> impl().set_title(frame, title) end)

  def set_min_size(frame, size),
    do: Helpers.with_wx_env(fn -> impl().set_min_size(frame, size) end)

  def set_icon(frame, icon), do: Helpers.with_wx_env(fn -> impl().set_icon(frame, icon) end)

  def set_menubar(frame, menubar),
    do: Helpers.with_wx_env(fn -> impl().set_menubar(frame, menubar) end)

  def iconize(frame, iconize), do: Helpers.with_wx_env(fn -> impl().iconize(frame, iconize) end)
  def shown?(frame), do: Helpers.with_wx_env(fn -> impl().shown?(frame) end)
  def active?(frame), do: Helpers.with_wx_env(fn -> impl().active?(frame) end)
  def raise_window(frame), do: Helpers.with_wx_env(fn -> impl().raise_window(frame) end)

  def update_apple_menu(title, frame, menubar) do
    Helpers.with_wx_env(fn -> impl().update_apple_menu(title, frame, menubar) end)
  end

  def new_menubar, do: Helpers.with_wx_env(fn -> impl().new_menubar() end)
  def on_crash_destroy(frame), do: Helpers.with_wx_env(fn -> impl().on_crash_destroy(frame) end)
  def close_event_veto(inev), do: Helpers.with_wx_env(fn -> impl().close_event_veto(inev) end)

  defp impl, do: Desktop.Platform.backend()
end
