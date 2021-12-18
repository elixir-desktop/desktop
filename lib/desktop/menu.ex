defmodule Desktop.Menu do
  @moduledoc """
  Menu module used to create and handle menus in Desktop
  
  Menues are defined similiar to Live View using a callback module an XML:
  
  ```
    defmodule ExampleMenuBar do
      use Desktop.Menu
  
      @impl true
      def mount(menu) do
        menu = assign(menu, items: ExampleRepo.all_items())
        {:ok, menu}
      end
  
      @impl true
      def handle_event(command, menu) do
        case command do
          <<"open">> -> :not_implemented
          <<"quit">> -> Desktop.Window.quit()
          <<"help">> -> :wx_misc.launchDefaultBrowser(\'https://google.com\')
          <<"about">> -> :not_implemented
        end
  
        {:noreply, menu}
      end
  
      @impl true
      def render(assigns) do
        ~E\"""
        <menubar>
          <menu label="<%= gettext "File" %>">
              <item onclick="open"><%= gettext "Open" %></item>
              <hr/>
              <item onclick="quit"><%= gettext "Quit" %></item>
          </menu>
          <menu label="<%= gettext "Items" %>">
            <%= for item <- @items do %>
              <item><%= item.name %></item>
            <% end %>
          </menu>
          <menu label="<%= gettext "Help" %>">
              <item onclick="help"><%= gettext "Show Documentation" %></item>
              <item onclick="about"><%= gettext "About" %></item>
          </menu>
        </menubar>
        \"""
      end
    end
  ```
  
  # Template
  
  As in live view the template can either be embedded in the `def render(assigns)`
  method or it can be side loaded as a .eex file next to the menues .ex file.
  
  # XML Structure
  
  These items are defined:
  
  ## `<menubar>...menues...</menubar>`
  
  For an application (window) menubar this must be the root element. When
  passing a menubar to `Desktop.Window` start parameters this has to be the root element.
  It has no attributes
  
  ## `<menu label="Label">...items...</menu>`
  
  For an icon menu `menu` must be the root element. Menu elements can contain multiple
  children of type `menu`, `item` and `hr`
  
  ### Attributes
  
  * `label` - the label that should be displayed on the menu
  
  ## `<item ...>Label</item>`
  
  This is an entry in the menu with a text a type and an onclick action
  
  ### Attributes
  
  * `onclick` - an event name that should be fired when this item is clicked. It will cause `handle_event/2` to be called
  * `type`        - the type of the item. The default is `normal`, but it can be either
    * `normal`    - a normal text item
    * `radio`     - a radio button
    * `checkbox`  - a checkbox item
  * `checked` - whether the `checkbox` or `radio` button should be checked. `nil`, `false` and `0` are treated
  as false values, every other value is treated as true.
  * `disabled` - whether the item should be disabled. `nil`, `false` and `0` are treated
  as false values, every other value is treated as true.
  
  ## `<hr />`
  
  A separator item
  
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

  @type t() :: %Menu{
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
    Module.register_attribute(__CALLER__.module, :is_menu_server, persist: true, accumulate: false)

    Module.put_attribute(__CALLER__.module, :is_menu_server, Keyword.get(opts, :server, true))

    # Phoenix.LiveView.HTMLEngine vs. Phoenix.HTML.Engine
    quote do
      @behaviour Desktop.Menu
      import Desktop.Menu, only: [assign: 2, connected?: 1]
      import Phoenix.HTML, only: [sigil_e: 2, sigil_E: 2]
      import Phoenix.LiveView.Helpers, only: [sigil_L: 2, sigil_H: 2]
      alias Desktop.Menu

      @before_compile Desktop.Menu
    end
  end

  defmacro __before_compile__(env) do
    render? = Module.defines?(env.module, {:render, 1})
    root = Path.dirname(env.file)
    filename = template_filename(env)
    templates = Phoenix.Template.find_all(root, filename)

    case {render?, templates} do
      {true, [template | _]} ->
        IO.warn(
          "ignoring template #{inspect(template)} because the Menu " <>
            "#{inspect(env.module)} defines a render/1 function",
          Macro.Env.stacktrace(env)
        )

        :ok

      {true, []} ->
        :ok

      {false, [template]} ->
        ext = template |> Path.extname() |> String.trim_leading(".") |> String.to_atom()
        engine = Map.fetch!(Phoenix.Template.engines(), ext)
        ast = engine.compile(template, filename)

        quote do
          @file unquote(template)
          @external_resource unquote(template)
          def render(var!(assigns)) when is_map(var!(assigns)) do
            unquote(ast)
          end
        end

      {false, [_ | _]} ->
        IO.warn(
          "multiple templates were found for #{inspect(env.module)}: #{inspect(templates)}",
          Macro.Env.stacktrace(env)
        )

        :ok

      {false, []} ->
        template = Path.join(root, filename <> ".heex")

        message = ~s'''
        render/1 was not implemented for #{inspect(env.module)}.
        
        Make sure to either explicitly define a render/1 clause with a Menu template:
        
            def render(assigns) do
              ~H"""
              ...
              """
            end
        
        Or create a file at #{inspect(template)} with the Menu template.
        '''

        IO.warn(message, Macro.Env.stacktrace(env))

        quote do
          @external_resource unquote(template)
          def render(_assigns) do
            raise unquote(message)
          end
        end
    end
  end

  defp template_filename(env) do
    env.module
    |> Module.split()
    |> List.last()
    |> Macro.underscore()
    |> Kernel.<>(".html")
  end

  def connected?(_menu), do: true
  def assign(menu, properties \\ [])

  def assign(menu, properties) when is_list(properties) do
    assign(menu, Map.new(properties))
  end

  def assign(menu = %Menu{assigns: assigns}, properties) when is_map(properties) do
    %Menu{menu | assigns: Map.merge(assigns, properties)}
  end

  def assign(menu, property, value) when is_atom(property) do
    assign(menu, %{property => value})
  end

  def assign_new(menu = %Menu{assigns: assigns}, property, fun)
      when is_atom(property) and is_function(fun) do
    %Menu{menu | assigns: Map.put_new_lazy(assigns, property, fun)}
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
    GenServer.start_link(Menu, init_opts, opts)
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

    if is_module_server?(module) do
      Process.register(menu_pid, module)
    end

    {:ok, menu}
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

  def get_icon(%Menu{pid: menu_pid}) when is_pid(menu_pid) do
    get_icon(menu_pid)
  end

  def get_icon(menu_pid) when is_pid(menu_pid) do
    GenServer.call(menu_pid, :get_icon)
  end

  def set_icon(%Menu{pid: menu_pid}, icon) when is_pid(menu_pid) do
    set_icon(menu_pid, icon)
  end

  def set_icon(menu_pid, icon) when is_pid(menu_pid) do
    if menu_pid == self() do
      spawn_link(Menu, :set_icon, [menu_pid, icon])
    else
      GenServer.call(menu_pid, {:set_icon, icon})
    end
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
    menu =
      with {:ok, {:noreply, menu}} <- invoke_module_func(module, :handle_event, [event, menu]),
           {:ok, _updated?, menu} <- update_dom(menu) do
        menu
      else
        _ -> menu
      end

    {:reply, menu.assigns, menu}
  end

  @impl true
  def handle_cast(:popup_menu, menu = %{__adapter__: adapter}) do
    adapter = Adapter.popup_menu(adapter)
    {:noreply, %{menu | __adapter__: adapter}}
  end

  @impl true
  def handle_cast(:recreate_menu, menu = %{__adapter__: adapter, dom: dom}) do
    # This is called from within the Adapter
    adapter = Adapter.recreate_menu(adapter, dom)
    {:noreply, %{menu | __adapter__: adapter}}
  end

  @impl true
  def handle_cast(:mount, menu) do
    {:noreply, do_mount(menu)}
  end

  @impl true
  def handle_info(event, menu = %{__adapter__: adapter = %{__struct__: adapter_module}})
      when is_tuple(event) and elem(event, 0) == :wx do
    {:noreply, adapter} = adapter_module.handle_info(event, adapter)

    {:noreply, %{menu | __adapter__: adapter}}
  end

  @impl true
  def handle_info(msg, menu) do
    {:noreply, proxy_handle_info(msg, menu)}
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

  defp do_mount(menu = %Menu{module: module}) do
    case invoke_module_func(module, :mount, [menu]) do
      {:ok, {:ok, menu}} ->
        case update_dom(menu) do
          {:ok, _updated?, menu} -> menu
          _error -> menu
        end

      _ ->
        menu
    end
  end

  defp proxy_handle_info(msg, menu = %Menu{module: module}) do
    with {:ok, {:noreply, menu}} <- invoke_module_func(module, :handle_info, [msg, menu]),
         {:ok, _updated?, menu} <- update_dom(menu) do
      menu
    else
      _ -> menu
    end
  end

  @spec update_dom(menu :: t()) :: {:ok, updated :: boolean(), menu :: t()} | {:error, binary()}
  defp update_dom(menu = %Menu{__adapter__: adapter, module: module, dom: dom, assigns: assigns}) do
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

  defp is_module_server?(module) do
    try do
      case Keyword.get(module.__info__(:attributes), :is_menu_server, false) do
        [true] -> true
        _ -> false
      end
    rescue
      _error -> false
    end
  end
end
