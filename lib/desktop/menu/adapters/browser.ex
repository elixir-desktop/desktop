defmodule Desktop.Menu.Adapter.Browser do
  @moduledoc false

  defstruct [:menu_pid]

  @type t() :: %__MODULE__{menu_pid: pid() | nil}

  def new(opts) do
    %__MODULE__{menu_pid: Keyword.get(opts, :menu_pid)}
  end

  def create(adapter, _dom), do: adapter
  def update_dom(adapter, _dom), do: adapter
  def popup_menu(adapter), do: adapter
  def recreate_menu(adapter, _dom), do: adapter
  def menubar(_adapter), do: nil
  def get_icon(_adapter), do: nil
  def set_icon(adapter, _icon), do: {:ok, adapter}
end
