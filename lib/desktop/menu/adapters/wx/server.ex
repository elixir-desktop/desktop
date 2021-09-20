defmodule Desktop.Menu.Adapter.Wx.Server do
  use GenServer
  require Record
  require Logger
  alias Desktop.Menu.Proxy

  for tag <- [:wx, :wxCommand] do
    Record.defrecordp(tag, Record.extract(tag, from_lib: "wx/include/wx.hrl"))
  end

  def start_link(menu, opts \\ []) do
    GenServer.start_link(__MODULE__, menu, opts)
  end

  def update(pid, menu) do
    GenServer.call(pid, {:update_menu, menu})
  end

  def get(pid) do
    GenServer.call(pid, :get_menu)
  end

  def update_callbacks(pid, callbacks) do
    GenServer.call(pid, {:update_callbacks, callbacks})
  end

  @impl true
  def init(menu) do
    {:ok, %{menu | server: self()}}
  end

  def handle_call({:update_callbacks, callbacks}, _from, %{env: env} = menu) do
    :wx.set_env(env)

    menu =
      :wx.batch(fn ->
        do_update_callbacks(menu, callbacks)
      end)

    {:reply, menu, menu}
  end

  @impl true
  def handle_call(:get_menu, _from, menu) do
    {:reply, menu, menu}
  end

  @impl true
  def handle_call({:update_menu, menu}, _from, _) do
    {:reply, menu, menu}
  end

  @impl true
  def handle_cast({:update_menu, menu}, _) do
    {:noreply, menu}
  end

  @impl true
  def handle_info(
        wx(id: id, event: wxCommand(type: :command_menu_selected)),
        menu = %{bindings: bindings, old_bindings: old, proxy: proxy}
      ) do
    Map.merge(old, bindings)
    |> Map.get(id)
    |> case do
      nil ->
        Logger.warning("Desktop.Menu unbound message id #{inspect(id)}")
        menu

      name ->
        Proxy.trigger_event(proxy, name)
    end

    {:noreply, menu}
  end

  @impl true
  def handle_info(event = wx(), menu) do
    Logger.warning("Desktop.Menu received unexpected wx message #{inspect({event, menu})}")
    {:noreply, menu}
  end

  defp do_update_callbacks(
         menu = %{bindings: bindings, old_bindings: old},
         callbacks
       ) do
    new_bindings =
      List.wrap(callbacks)
      |> List.flatten()
      |> Enum.reduce(%{}, fn callback, bind ->
        case callback do
          {:connect, {event_src, id, onclick}} ->
            :wxMenu.connect(event_src, :command_menu_selected, id: id)
            Map.put(bind, id, onclick)

          _ ->
            bind
        end
      end)

    {bindings, old} =
      if map_size(bindings) > 1000 do
        {new_bindings, bindings}
      else
        {Map.merge(bindings, new_bindings), old}
      end

    %{menu | bindings: bindings, old_bindings: old}
  end
end
