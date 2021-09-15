defmodule Desktop.Menu do
  alias Desktop.{Menu, Wx, OS, Fallback}
  alias Desktop.Menu.{Adapter, Parser}

  defstruct [
    :__adapter__,
    :assigns,
    :dom,
    :mod
  ]

  @type t() :: %__MODULE__{
          __adapter__: any(),
          assigns: %{},
          mod: atom(),
          dom: any()
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

  def assign(%Menu{assigns: assigns} = menu, keywords \\ []) do
    assigns = Map.merge(assigns, Map.new(keywords))
    %{menu | assigns: assigns}
  end

  def new(module, env, adapter \\ :wx)

  def new(module, env, :wx) do
    new(module, env, Adapter.Wx)
  end

  def new(module, env, :dbus) do
    new(module, env, Adapter.DBus)
  end

  def new(module, env, adapter) do
    adapter = adapter.new(env, module)

    menu = %Menu{
      __adapter__: adapter,
      mod: module,
      dom: [],
      assigns: %{}
    }
  end

  def create(%{__adapter__: nil}) do
    raise "Menu has no adapter set"
  end

  def create(%{__adapter__: adapter, mod: mod, dom: dom} = menu, opts) do
    menu = %{menu | __adapter__: Adapter.create(adapter, dom, opts)}
    {:ok, menu} = mod.mount(menu)
    update_dom(menu)
  end

  def update_dom(%{__adapter__: nil}) do
    raise "Menu has no adapter"
  end

  def update_dom(%{__adapter__: adapter, mod: mod, dom: dom, assigns: assigns} = menu) do
    new_dom =
      mod.render(assigns)
      |> Parser.parse()

    if new_dom != dom do
      adapter = Adapter.update_dom(adapter, new_dom)
      %{menu | __adapter__: adapter, dom: new_dom}
    else
      menu
    end
  end

  def popup_menu(%{__adapter__: nil}) do
    raise "Menu has no adapter"
  end

  def popup_menu(%{__adapter__: adapter, dom: dom}) do
    Adapter.popup_menu(adapter, dom)
  end

  def menubar(%{__adapter__: nil}) do
    raise "Menu has no adapter"
  end

  def menubar(%{__adapter__: adapter}) do
    Adapter.menubar(adapter)
  end

  # def start_link(mod, wx_env, bar) do
  #   GenServer.start_link(__MODULE__, [mod, wx_env, bar], name: mod)
  # end

  # @impl true
  # def init([mod, wx_env, bar]) do
  #   :wx.set_env(wx_env)

  #   pid = self()
  #   menu = %Menu{mod: mod, assigns: %{}, bindings: %{}, old_bindings: %{}, pid: pid}

  #   menu =
  #     case bar do
  #       {:taskbar, icon} ->
  #         create_popup = fn -> create_popup_menu(pid) end
  #         %Menu{menu | taskbar: Fallback.taskbaricon_new_wx(create_popup, icon)}

  #       wx_ref ->
  #         :wxMenuBar = :wx.getObjectType(wx_ref)
  #         %Menu{menu | menubar: bar}
  #     end

  #   {:ok, menu} = mod.mount(menu)
  #   {:ok, update_dom(menu)}
  # end

  # defp create_popup_menu(pid) do
  #   {time, value} = :timer.tc(fn -> menu(pid) end)
  #   if time > 100_000, do: Logger.warning("create_popup took #{div(time, 1000)}ms")
  #   value
  # end

  # defp update_dom(menu = %Menu{mod: mod, assigns: assigns, menubar: menubar, pid: pid}) do
  #   spawn_link(fn ->
  #     new_dom = mod.render(assigns) |> parse()
  #     dom = Desktop.Env.put({:dom, pid}, new_dom)

  #     if dom != new_dom do
  #       if menubar != nil do
  #         :wx.set_env(Desktop.Env.wx_env())
  #         menues = :wx.batch(fn -> do_create_menubar_menues(pid, new_dom) end)
  #         GenServer.cast(pid, {:update_menubar, menues})
  #       else
  #         create_menu(pid)
  #       end
  #     end
  #   end)

  #   menu
  # end

  # @impl true
  # def handle_call(:menubar, from, menu = %Menu{menubar: menubar, loaded: loaded}) do
  #   if loaded == true do
  #     {:reply, menubar, menu}
  #   else
  #     {:noreply, %Menu{menu | loaded: {:from, from}}}
  #   end
  # end

  # @impl true
  # def handle_call(:taskbar, _from, menu = %Menu{taskbar: taskbar}) do
  #   {:reply, taskbar, menu}
  # end

  # @impl true
  # def handle_call({:update_callbacks, callbacks}, _from, menu) do
  #   menu =
  #     :wx.batch(fn ->
  #       update_callbacks(menu, callbacks)
  #     end)

  #   {:reply, :ok, menu}
  # end

  # defp update_callbacks(menu = %Menu{bindings: bindings, old_bindings: old}, callbacks) do
  #   new_bindings =
  #     List.wrap(callbacks)
  #     |> List.flatten()
  #     |> Enum.reduce(%{}, fn callback, bind ->
  #       case callback do
  #         {:connect, {event_src, id, onclick}} ->
  #           :wxMenu.connect(event_src, :command_menu_selected, id: id)
  #           Map.put(bind, id, onclick)

  #         _ ->
  #           bind
  #       end
  #     end)

  #   {bindings, old} =
  #     if map_size(bindings) > 1000 do
  #       {new_bindings, bindings}
  #     else
  #       {Map.merge(bindings, new_bindings), old}
  #     end

  #   %Menu{menu | bindings: bindings, old_bindings: old}
  # end

  # @impl true
  # def handle_cast(:popup_menu, menu = %Menu{taskbar: bar}) do
  #   :wxTaskBarIcon.popupMenu(bar, create_popup_menu(self()))
  #   {:noreply, menu}
  # end

  # @impl true
  # def handle_cast({:update_callbacks, callbacks}, menu) do
  #   menu =
  #     :wx.batch(fn ->
  #       update_callbacks(menu, callbacks)
  #     end)

  #   {:noreply, menu}
  # end

  # @impl true
  # def handle_cast({:update_menubar, menues}, menu = %Menu{menubar: bar, loaded: loaded}) do
  #   menu =
  #     :wx.batch(fn ->
  #       do_update_menubar(menu, menues)
  #     end)

  #   case loaded do
  #     {:from, from} -> GenServer.reply(from, bar)
  #     _ -> :ok
  #   end

  #   {:noreply, %Menu{menu | loaded: true}}
  # end

  # @impl true
  # def handle_cast({:destroy, wx}, menu) do
  #   :wxMenu.destroy(wx)
  #   {:noreply, menu}
  # end

  # @impl true
  # def handle_info(
  #       wx(id: id, event: wxCommand(type: :command_menu_selected)),
  #       menu = %Menu{bindings: bindings, old_bindings: old, mod: mod}
  #     ) do
  #   menu =
  #     Map.merge(old, bindings)
  #     |> Map.get(id)
  #     |> case do
  #       nil ->
  #         Logger.warning("Desktop.Menu unbound message id #{inspect(id)}")
  #         menu

  #       name ->
  #         {:noreply, menu} = mod.handle_event(name, menu)
  #         update_dom(menu)
  #     end

  #   {:noreply, menu}
  # end

  # @impl true
  # def handle_info(event = wx(), menu) do
  #   Logger.warning("Desktop.Menu received unexpected wx message #{inspect({event, menu})}")
  #   {:noreply, menu}
  # end

  # @impl true
  # def handle_info(other, menu = %Menu{mod: mod}) do
  #   {:noreply, menu} = mod.handle_info(other, menu)
  #   {:noreply, update_dom(menu)}
  # end

  # def popup_menu(pid) do
  #   GenServer.cast(pid, :popup_menu)
  # end

  # def menubar(pid) do
  #   GenServer.call(pid, :menubar, :infinity)
  # end

  # def taskbar(pid) do
  #   GenServer.call(pid, :taskbar, :infinity)
  # end

  # def menu(pid) when is_atom(pid) do
  #   menu(Process.whereis(pid))
  # end

  # def menu(pid) do
  #   # This will create the menu for next round
  #   spawn(fn -> create_menu(pid) end)
  #   # This should return the current menu
  #   Desktop.Env.pop({:menu, pid}) || raise "No menu"
  # end

  # defp create_menu(pid) do
  #   dom = Desktop.Env.await({:dom, pid})
  #   :wx.set_env(Desktop.Env.wx_env())

  #   {menu, callbacks} =
  #     :wx.batch(fn ->
  #       menu = :wxMenu.new()
  #       callbacks = do_create_menu(pid, [menu], dom, OS.windows?())
  #       {menu, callbacks}
  #     end)

  #   # Would like to do this synchronously, but this is running in the context of
  #   # :wxe_server and so :wxe_server is blocked to accept :connect() calls
  #   GenServer.cast(pid, {:update_callbacks, callbacks})

  #   case Desktop.Env.put({:menu, pid}, menu) do
  #     nil -> :ok
  #     menu -> GenServer.cast(pid, {:destroy, menu})
  #   end
  # end

  # defp do_create_menubar_menues(pid, dom) when is_list(dom) do
  #   Enum.filter(dom, fn {tag, _attr, _content} -> tag == :menu end)
  #   |> Enum.map(fn {:menu, attr, content} ->
  #     menu = :wxMenu.new()
  #     callbacks = do_create_menu(pid, [menu], content, false)
  #     label = String.to_charlist(attr[:label] || "")
  #     {label, menu, callbacks}
  #   end)
  # end

  # defp do_update_menubar(menu = %Menu{menubar: menubar}, menues) do
  #   size = :wxMenuBar.getMenuCount(menubar)

  #   Enum.with_index(menues)
  #   |> Enum.map(fn {{label, menu, _callbacks}, pos} ->
  #     if pos < size do
  #       :wxMenuBar.replace(menubar, pos, menu, label)
  #     else
  #       :wxMenuBar.append(menubar, menu, label)
  #     end
  #   end)

  #   if length(menues) < size do
  #     for pos <- length(menues)..(size - 1), do: :wxMenuBar.remove(menubar, pos)
  #   end

  #   callbacks = Enum.map(menues, fn {_label, _menu, callback} -> callback end)
  #   update_callbacks(menu, callbacks)
  # end

  # defp do_create_menu(pid, menues, dom, invert) when is_list(dom) do
  #   if(invert, do: Enum.reverse(dom), else: dom)
  #   |> Enum.map(fn e -> do_create_menu(pid, menues, e, invert) end)
  # end

  # defp do_create_menu(pid, menues, dom, invert) when is_tuple(dom) do
  #   case dom do
  #     {:hr, _attr, _content} ->
  #       :wxMenu.appendSeparator(hd(menues))

  #     {:item, attr, content} ->
  #       kind =
  #         case attr[:type] do
  #           "checkbox" -> Wx.wxITEM_CHECK()
  #           "separator" -> Wx.wxITEM_SEPARATOR()
  #           "radio" -> Wx.wxITEM_RADIO()
  #           _other -> Wx.wxITEM_NORMAL()
  #         end

  #       item = :wxMenuItem.new(id: Wx.wxID_ANY(), text: List.flatten(content), kind: kind)
  #       id = :wxMenuItem.getId(item)
  #       :wxMenu.append(hd(menues), item)

  #       if attr[:checked] != nil do
  #         :wxMenuItem.check(item, check: is_true(attr[:checked]))
  #       end

  #       if is_true(attr[:disabled]) do
  #         :wxMenu.enable(hd(menues), id, false)
  #       end

  #       if attr[:onclick] != nil do
  #         event_src = if OS.windows?(), do: List.last(menues), else: hd(menues)

  #         # Wx keeps track of the calling process ot the connect call, so we collect
  #         # all connect's and call them from the GenServer itself.
  #         {:connect, {event_src, id, attr[:onclick]}}
  #       end

  #     {:menu, attr, content} ->
  #       menu = :wxMenu.new()
  #       ret = do_create_menu(pid, [menu | menues], content, invert)
  #       :wxMenu.append(hd(menues), Wx.wxID_ANY(), String.to_charlist(attr[:label] || ""), menu)
  #       ret
  #   end
  # end

  # def is_true(value) do
  #   value != nil and value != "false" and value != "0"
  # end

  # for tag <- [:xmlElement, :xmlAttribute, :xmlText] do
  #   Record.defrecordp(tag, Record.extract(tag, from_lib: "xmerl/include/xmerl.hrl"))
  # end

  # defp parse({:safe, string}) do
  #   parse(string)
  # end

  # defp parse(string) do
  #   string = unicode(string)

  #   try do
  #     {xml, []} = :xmerl_scan.string(string, encoding: :ref)

  #     # 'simple-form' is what xmerl documentation calls a tree of tuples:
  #     # {tag, attributes = %{key => value}, content = []}
  #     simple_form(xmlElement(xml, :content))
  #   catch
  #     :exit, error ->
  #       Logger.error("Failed to parse document #{inspect(error)}")

  #       []
  #   end
  # end

  # defp simple_form([]) do
  #   []
  # end

  # defp simple_form([element | rest]) do
  #   case simple_form(element) do
  #     :skip -> simple_form(rest)
  #     ret -> [ret | simple_form(rest)]
  #   end
  # end

  # defp simple_form(xmlElement(name: name, attributes: attr, content: children)) do
  #   attr =
  #     Enum.map(attr, fn xmlAttribute(name: tname, value: value) ->
  #       {tname, List.to_string(value)}
  #     end)
  #     |> Map.new()

  #   {name, attr, simple_form(children)}
  # end

  # defp simple_form(xmlText(value: value)) do
  #   List.to_string(value)
  #   |> String.trim_trailing()
  #   |> case do
  #     "" -> :skip
  #     other -> other
  #   end
  # end
end
