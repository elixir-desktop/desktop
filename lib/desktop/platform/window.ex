defmodule Desktop.Platform.Window do
  @moduledoc false

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
  @callback is_shown?(handle()) :: boolean()
  @callback is_active?(handle()) :: boolean()
  @callback raise_window(handle()) :: :ok
  @callback update_apple_menu(String.t(), handle(), term()) :: :ok
  @callback new_menubar() :: term()
  @callback on_crash_destroy(handle()) :: :ok
  @callback close_event_veto(term()) :: :ok

  def open(opts), do: impl().open(opts)
  def destroy(frame), do: impl().destroy_frame(frame)
  def connect(frame, event, fun), do: impl().connect(frame, event, fun)
  def show(frame, opts \\ []), do: impl().show(frame, opts)
  def hide(frame), do: impl().hide(frame)
  def set_title(frame, title), do: impl().set_title(frame, title)
  def set_min_size(frame, size), do: impl().set_min_size(frame, size)
  def set_icon(frame, icon), do: impl().set_icon(frame, icon)
  def set_menubar(frame, menubar), do: impl().set_menubar(frame, menubar)
  def iconize(frame, iconize), do: impl().iconize(frame, iconize)
  def is_shown?(frame), do: impl().is_shown?(frame)
  def is_active?(frame), do: impl().is_active?(frame)
  def raise_window(frame), do: impl().raise_window(frame)

  def update_apple_menu(title, frame, menubar),
    do: impl().update_apple_menu(title, frame, menubar)

  def new_menubar, do: impl().new_menubar()
  def on_crash_destroy(frame), do: impl().on_crash_destroy(frame)
  def close_event_veto(inev), do: impl().close_event_veto(inev)

  defp impl, do: Desktop.Platform.backend()
end
