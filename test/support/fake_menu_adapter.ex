defmodule Desktop.Menu.Adapter.Fake do
  @moduledoc false

  defstruct [:menu_pid, :notify, :taskbar_icon]

  def new(opts) do
    tray =
      case Keyword.get(opts, :wx) do
        {:taskbar, _} -> :tray
        _ -> nil
      end

    %__MODULE__{
      menu_pid: Keyword.get(opts, :menu_pid),
      notify: Keyword.get(opts, :notify),
      taskbar_icon: tray
    }
  end

  def create(adapter, _dom), do: adapter
  def update_dom(adapter, _dom), do: adapter
  def popup_menu(adapter), do: adapter
  def recreate_menu(adapter, _dom), do: adapter
  def menubar(_adapter), do: nil
  def get_icon(%{taskbar_icon: icon}), do: icon

  def set_icon(adapter, nil) do
    if is_pid(adapter.notify) and adapter.taskbar_icon != nil do
      send(adapter.notify, {:tray_destroyed, adapter.taskbar_icon})
    end

    {:ok, %{adapter | taskbar_icon: nil}}
  end

  def set_icon(adapter, icon), do: {:ok, %{adapter | taskbar_icon: icon}}
end
