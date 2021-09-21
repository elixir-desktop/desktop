defmodule Desktop.Menu do
  use GenServer

  require Logger
  alias Desktop.Menu
  alias Desktop.Menu.{Adapter, Parser}

  defstruct [
    :__adapter__,
    :assigns,
    :module,
    :dom,
    :pid
  ]

  @type t() :: %__MODULE__{
          __adapter__: any(),
          assigns: %{},
          module: module,
          dom: any(),
          pid: nil | pid()
        }

  @callback mount(Menu.t()) :: {:ok, Menu.t()}
  @callback handle_event(String.t(), Menu.t()) :: {:noreply, Menu.t()}
  @callback handle_info(any(), Menu.t()) :: {:noreply, Menu.t()}
  @callback render(Keyword.t()) :: String.t()

  @doc false
  defmacro __using__(opts) do
    quote do
      @behaviour Desktop.Menu
      import Desktop.Menu, only: [assign: 2, escape: 1]
      import Phoenix.HTML, only: [sigil_e: 2, sigil_E: 2]

      if not Keyword.get(unquote(opts), :skip_render, false) do
        require EEx
        @template Path.basename(__ENV__.file, ".ex") <> ".eex"
        EEx.function_from_file(:def, :render, "#{__DIR__}/#{@template}", [:assigns])
        defoverridable render: 1
      end
    end
  end

  def escape(string) do
    Parser.escape(string)
  end

  def assign(menu = %Menu{assigns: assigns}, keywords \\ []) do
    assigns = Map.merge(assigns, Map.new(keywords))
    %{menu | assigns: assigns}
  end

  def update_dom(%{__adapter__: nil}) do
    raise "Menu has no adapter"
  end

  def update_dom(menu = %{__adapter__: adapter, module: module, dom: dom, assigns: assigns}) do
    new_dom =
      module.render(assigns)
      |> Parser.parse()

    if new_dom != dom do
      adapter = Adapter.update_dom(adapter, new_dom)
      %{menu | __adapter__: adapter, dom: new_dom}
    else
      menu
    end
  end

  # GenServer implementation

  def start!(init_opts \\ [], opts \\ [])

  def start!(init_opts, opts) do
    case start_link(init_opts, opts) do
      {:ok, pid} -> pid
      {:error, {:already_started, pid}} -> pid
      {:error, reason} -> raise reason
      :ignore -> nil
    end
  end

  @spec start_link(keyword(), keyword()) :: GenServer.on_start()
  def start_link(init_opts \\ [], opts \\ [])

  def start_link(init_opts, opts) do
    GenServer.start_link(__MODULE__, init_opts, opts)
  end

  @impl true
  def init(init_opts) do
    menu_pid = self()
    module = Keyword.get(init_opts, :module)
    dom = Keyword.get(init_opts, :dom, [])

    adapter_module =
      case Keyword.get(init_opts, :adapter, Adapter.Wx) do
        mod when mod in [Adapter.Wx, Adapter.DBus] -> mod
        _ -> Adapter.Wx
      end

    adapter_opts =
      init_opts
      |> Keyword.drop([:dom, :adapter])
      |> Keyword.put(:menu_pid, menu_pid)

    adapter =
      adapter_opts
      |> adapter_module.new()
      |> Adapter.create(dom)

    menu =
      %Menu{
        __adapter__: adapter,
        module: module,
        dom: dom,
        assigns: %{},
        pid: menu_pid
      }
      |> do_mount()

    {:ok, menu}
  end

  def mount(menu_pid) do
    GenServer.call(menu_pid, :mount)
  end

  def trigger_event(menu_pid, event) do
    GenServer.call(menu_pid, {:trigger_event, event})
  end

  def popup_menu(menu_pid) do
    GenServer.call(menu_pid, :popup_menu)
  end

  def menubar(menu_pid) do
    GenServer.call(menu_pid, :menubar)
  end

  @impl true
  def handle_call(:popup_menu, _from, menu = %{__adapter__: adapter, dom: dom}) do
    {:reply, Adapter.popup_menu(adapter, dom), menu}
  end

  @impl true
  def handle_call(:menubar, _from, menu = %{__adapter__: adapter}) do
    {:reply, Adapter.menubar(adapter), menu}
  end

  @impl true
  def handle_call({:trigger_event, event}, _from, menu = %{module: module}) do
    menu = try_module_func(module, :handle_event, [event, menu], menu)

    {:reply, menu, menu}
  end

  def handle_call(:mount, _from, menu) do
    menu = do_mount(menu)

    {:reply, menu, menu}
  end

  @impl true
  def handle_info(:changed, menu) do
    {:noreply, do_changed(menu)}
  end

  @impl true
  def handle_info(event, menu = %{__adapter__: adapter = %{__struct__: adapter_module}}) do
    adapter =
      case elem(event, 0) do
        :wx ->
          {:noreply, adapter} = adapter_module.handle_info(event, adapter)
          adapter

        _ ->
          adapter
      end

    {:noreply, %{menu | __adapter__: adapter}}
  end

  # Private functions

  defp do_mount(menu = %{module: module}) do
    try_module_func(module, :mount, [menu], menu)
  end

  defp do_changed(menu = %{module: module}) do
    try_module_func(module, :handle_info, [:changed, menu], menu)
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

  defp maybe_update(menu = %{assigns: assigns}, %{assigns: assigns}) do
    menu
  end

  defp maybe_update(menu = %{}, %{assigns: assigns}) do
    Menu.update_dom(%{menu | assigns: assigns})
  end

  defp maybe_update(menu, _) do
    menu
  end
end
