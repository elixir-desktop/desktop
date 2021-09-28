defmodule Desktop.Menu do
  @moduledoc """
  Menu module used to create and handle menus in Desktop
  """
  use GenServer

  require Logger
  alias Desktop.Menu
  alias Desktop.Menu.{Adapter, Parser}

  defstruct [
    :__adapter__,
    :app,
    :assigns,
    :module,
    :dom,
    :pid,
    :last_render
  ]

  @type t() :: %__MODULE__{
          __adapter__: any(),
          app: nil,
          assigns: %{},
          module: module,
          dom: any(),
          pid: nil | pid(),
          last_render: nil | DateTime.t()
        }

  @callback mount(assigns :: map()) :: {:ok, map()}
  @callback handle_event(event_name :: String.t(), assigns :: map()) :: {:noreply, map()}
  @callback handle_info(any(), assigns :: map()) :: {:noreply, map()}
  @callback render(Keyword.t()) :: String.t()

  @doc false
  defmacro __using__(opts) do
    quote do
      @behaviour Desktop.Menu
      import Desktop.Menu, only: [assign: 2, escape: 1]
      import Phoenix.HTML, only: [sigil_e: 2, sigil_E: 2]
      alias Desktop.Menu

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

  def assign(assigns = %{}) do
    assigns
  end

  def assign(assigns = %{}, properties) when is_list(properties) do
    assign(assigns, Map.new(properties))
  end

  def assign(assigns = %{}, properties) when is_map(properties) do
    assigns
    |> Map.merge(properties)
  end

  def assign(assigns = %{}, property, value) when is_atom(property) do
    assigns
    |> Map.put(property, value)
  end

  def assign_new(assigns = %{}, property, value) when is_atom(property) do
    assigns
    |> Map.put_new(property, value)
  end

  def assign_new(assigns = %{}, property, fun) when is_function(fun) do
    assigns
    |> Map.put_new_lazy(property, fun)
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
    app = Keyword.get(init_opts, :app, nil)

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
        app: app,
        module: module,
        dom: dom,
        assigns: %{},
        pid: menu_pid
      }
      |> do_mount()

    {:ok, menu}
  end

  # def mount(menu_pid) do
  #   GenServer.cast(menu_pid, :mount)
  # end

  def trigger_event(menu_pid, event) do
    GenServer.call(menu_pid, {:trigger_event, event})
  end

  def popup_menu(menu_pid) do
    GenServer.call(menu_pid, :popup_menu)
  end

  def menubar(menu_pid) do
    GenServer.call(menu_pid, :menubar)
  end

  def get_icon(%{__menu__: menu_pid}) when is_pid(menu_pid) do
    get_icon(menu_pid)
  end

  def get_icon(menu_pid) when is_pid(menu_pid) do
    GenServer.call(menu_pid, :get_icon)
  end

  def set_icon(%{__menu__: menu_pid}, icon) when is_pid(menu_pid) do
    set_icon(menu_pid, icon)
  end

  def set_icon(menu_pid, icon) when is_pid(menu_pid) do
    if menu_pid == self() do
      spawn_link(__MODULE__, :set_icon, [menu_pid, icon])
    else
      GenServer.call(menu_pid, {:set_icon, icon})
    end
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
  def handle_call(:get_icon, _from, menu) do
    {:reply, get_adapter_icon(menu), menu}
  end

  @impl true
  def handle_call({:set_icon, icon}, _from, menu) do
    case set_adapter_icon(menu, icon) do
      {:ok, menu} -> {:reply, get_adapter_icon(menu), menu}
      error -> {:reply, error, menu}
    end
  end

  @impl true
  def handle_call({:trigger_event, event}, _from, menu = %{module: module}) do
    assigns = build_assigns(menu)

    menu =
      case invoke_module_func(module, :handle_event, [event, assigns]) do
        {:ok, {:ok, assigns}} -> maybe_update_dom(menu, assigns)
        {:ok, {:noreply, assigns}} -> maybe_update_dom(menu, assigns)
        _ -> menu
      end

    {:reply, build_assigns(menu), menu}
  end

  @impl true
  def handle_cast(:mount, menu) do
    {:noreply, do_mount(menu)}
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

  defp get_adapter_icon(%{__adapter__: adapter}) do
    Adapter.get_icon(adapter)
  end

  defp set_adapter_icon(menu = %{app: app}, {:file, icon}) do
    with {:ok, wx_icon} <- Desktop.Image.new_icon(app, icon),
         ret = {:ok, _menu} <- set_adapter_icon(menu, wx_icon) do
      # Destroy the :wxIcon
      Desktop.Image.destroy(wx_icon)
      # Now return the result
      ret
    end
  end

  defp set_adapter_icon(menu = %{__adapter__: adapter}, icon) do
    with {:ok, adapter} <- Adapter.set_icon(adapter, icon) do
      menu = %{menu | __adapter__: adapter}
      {:ok, menu}
    end
  end

  defp do_mount(menu = %__MODULE__{module: module}) do
    assigns = build_assigns(menu)

    case invoke_module_func(module, :mount, [assigns]) do
      {:ok, {:ok, assigns}} -> maybe_update_dom(menu, assigns)
      {:ok, {:noreply, assigns}} -> maybe_update_dom(menu, assigns)
      _ -> menu
    end
  end

  defp do_changed(menu = %__MODULE__{module: module}) do
    assigns = build_assigns(menu)

    case invoke_module_func(module, :handle_info, [:changed, assigns]) do
      {:ok, {:ok, assigns}} -> maybe_update_dom(menu, assigns)
      {:ok, {:noreply, assigns}} -> maybe_update_dom(menu, assigns)
      _ -> menu
    end
  end

  defp maybe_update_dom(menu, assigns = %{__menu__: _}) do
    maybe_update_dom(menu, Map.delete(assigns, :__menu__))
  end

  defp maybe_update_dom(menu = %{assigns: assigns}, assigns) do
    menu
  end

  defp maybe_update_dom(menu = %{}, assigns) do
    case update_dom(%{menu | assigns: assigns}) do
      {:ok, _updated?, menu} ->
        menu

      _error ->
        menu
    end
  end

  defp maybe_update_dom(menu, _) do
    menu
  end

  @spec update_dom(menu :: t()) :: {:ok, updated :: boolean(), menu :: t()} | {:error, binary()}
  defp update_dom(
         menu = %__MODULE__{__adapter__: adapter, module: module, dom: dom, assigns: assigns}
       ) do
    with {:ok, new_dom} <- invoke_render(module, assigns) do
      if new_dom != dom do
        adapter = Adapter.update_dom(adapter, new_dom)
        {:ok, true, %{menu | __adapter__: adapter, dom: new_dom, last_render: DateTime.utc_now()}}
      else
        {:ok, false, menu}
      end
    end
  end

  @spec invoke_render(module :: module(), assigns :: map()) ::
          {:ok, any()} | {:error, binary()}
  defp invoke_render(module, assigns) do
    with {:ok, str_render} <- invoke_module_func(module, :render, [assigns]) do
      {:ok, Parser.parse(str_render)}
    end
  end

  @spec invoke_module_func(module :: module(), func :: atom(), args :: list(any())) ::
          {:error, binary()} | {:ok, any()}
  defp invoke_module_func(module, func, args) do
    try do
      Kernel.apply(module, func, args)
    rescue
      error ->
        Logger.error(Exception.format(:error, error, __STACKTRACE__))
        {:error, "Failed to invoke #{module}.#{func}/#{Enum.count(args)}"}
    else
      return -> {:ok, return}
    end
  end

  defp build_assigns(%__MODULE__{assigns: assigns, pid: menu_pid}) do
    assigns
    |> Map.merge(%{__menu__: menu_pid})
  end
end
