defmodule Desktop.Menu.Proxy do
  alias Desktop.Menu
  use GenServer
  require Logger

  @spec start_link(nil | Menu.t(), keyword()) :: GenServer.on_start()
  def start_link(menu, opts \\ [])

  def start_link(nil, opts) do
    GenServer.start_link(__MODULE__, nil, opts)
  end

  def start_link(%Menu{} = menu, opts) do
    GenServer.start_link(__MODULE__, menu, opts)
  end

  @impl true
  def init(nil) do
    {:ok, nil}
  end

  def init(%Menu{} = menu) do
    {:ok, %{menu | proxy: self()}}
  end

  def mount(proxy) do
    GenServer.call(proxy, :mount)
  end

  def get(proxy) do
    GenServer.call(proxy, :get)
  end

  def update(proxy, %Menu{} = menu) do
    GenServer.call(proxy, {:update, menu})
  end

  def trigger_event(proxy, event) do
    GenServer.call(proxy, {:trigger_event, event})
  end

  @impl true
  def handle_call({:update, menu}, _from, _) do
    {:reply, menu, menu}
  end

  @impl true
  def handle_call(:get, _from, menu) do
    {:reply, menu, menu}
  end

  @impl true
  def handle_call({:trigger_event, event}, _from, menu) do
    menu = Menu.trigger_event(menu, event)
    {:reply, menu, menu}
  end

  def handle_call(:mount, _from, %{mod: mod} = menu) do
    menu = try_module_func(mod, :mount, [menu], menu)

    {:reply, menu, menu}
  end

  @impl true
  def handle_info(:changed, %{mod: mod} = menu) do
    menu = try_module_func(mod, :handle_info, [:changed, menu], menu)

    {:noreply, menu}
  end

  defp try_module_func(module, func, args, menu) do
    try do
      Kernel.apply(module, func, args)
    rescue
      error ->
        Logger.debug(error)
        menu
    else
      {:ok, ret} -> maybe_update(menu, ret)
      {:noreply, ret} -> maybe_update(menu, ret)
      _ -> menu
    end
  end

  defp maybe_update(%{assigns: assigns} = menu, %{assigns: assigns}) do
    menu
  end

  defp maybe_update(%{} = menu, %{assigns: assigns}) do
    Menu.update_dom(%{menu | assigns: assigns})
  end

  defp maybe_update(menu, _) do
    menu
  end
end
