defmodule Desktop.Menu do
  alias Desktop.{Menu, Wx, OS}
  require Record
  require Logger
  use GenServer
  defstruct [:assigns, :dom, :mod, :bindings, :menubar, :taskbar, :pid, :last_dom]

  @type t() :: %__MODULE__{
          assigns: %{},
          mod: Atom.t(),
          bindings: %{},
          pid: pid()
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

      if not Keyword.get(unquote(opts), :skip_render, false) do
        require EEx
        @template Path.basename(__ENV__.file, ".ex") <> ".eex"
        EEx.function_from_file(:def, :render, "#{__DIR__}/#{@template}", [:assigns])
        defoverridable render: 1
      end
    end
  end

  def assign(menu = %Menu{assigns: assigns}, keywords \\ []) do
    assigns = Map.merge(assigns, Map.new(keywords))
    %Menu{menu | assigns: assigns}
  end

  def start_link(mod, wx_env, bar) do
    GenServer.start_link(__MODULE__, [mod, wx_env, bar], name: mod)
  end

  @impl true
  def init([mod, wx_env, bar]) do
    :wx.set_env(wx_env)

    menu = %Menu{mod: mod, assigns: %{}, bindings: %{}, pid: self()}

    menu =
      case :wx.getObjectType(bar) do
        :wxTaskBarIcon -> %Menu{menu | taskbar: bar}
        :wxMenuBar -> %Menu{menu | menubar: bar}
      end

    {:ok, menu} = mod.mount(menu)
    {:ok, update_dom(menu)}
  end

  defp update_dom(menu = %Menu{mod: mod, assigns: assigns, menubar: menubar, pid: pid}) do
    spawn(fn ->
      new_dom = mod.render(assigns) |> parse()
      dom = Desktop.Env.put({:dom, pid}, new_dom)

      if menubar != nil and dom != new_dom do
        :wx.set_env(Desktop.Env.wx_env())

        menues =
          :wx.batch(fn ->
            do_create_menubar_menues(pid, dom)
          end)

        GenServer.cast(pid, {:update_menubar, menues})
      end
    end)

    menu
  end

  @impl true
  def handle_call(:menubar, _from, menu = %Menu{menubar: menubar}) do
    {:reply, menubar, menu}
  end

  @impl true
  def handle_call({:update_callbacks, callbacks}, _from, menu) do
    bindings =
      :wx.batch(fn ->
        update_callbacks(callbacks)
      end)

    {:reply, :ok, %Menu{menu | bindings: bindings}}
  end

  defp update_callbacks(callbacks) do
    List.wrap(callbacks)
    |> List.flatten()
    |> Enum.reduce(%{}, fn callback, bind ->
      case callback do
        {:connect, {event_src, id, onclick}} ->
          IO.puts("connect: #{inspect({event_src, id, onclick})}")
          :wxMenu.connect(event_src, :command_menu_selected, id: id)
          Map.put(bind, id, onclick)

        _ ->
          bind
      end
    end)
  end

  @impl true
  def handle_cast({:update_callbacks, callbacks}, menu) do
    bindings =
      :wx.batch(fn ->
        update_callbacks(callbacks)
      end)

    {:noreply, %Menu{menu | bindings: bindings}}
  end

  @impl true
  def handle_cast({:update_menubar, menues}, menu) do
    menu =
      :wx.batch(fn ->
        do_update_menubar(menu, menues)
      end)

    {:noreply, menu}
  end

  for tag <- [:wx, :wxCommand] do
    Record.defrecordp(tag, Record.extract(tag, from_lib: "wx/include/wx.hrl"))
  end

  @impl true
  def handle_info(
        wx(id: id, event: wxCommand(type: :command_menu_selected)),
        menu = %Menu{bindings: bindings, mod: mod}
      ) do
    menu =
      case Map.get(bindings, id) do
        nil ->
          Logger.warning("Desktop.Menu unbound message id #{inspect(id)}", [menu])
          menu

        name ->
          {:noreply, menu} = mod.handle_event(name, menu)
          update_dom(menu)
      end

    {:noreply, menu}
  end

  @impl true
  def handle_info(event = wx(), menu) do
    Logger.warning("Desktop.Menu received unexpected wx message", [event, menu])
    {:noreply, menu}
  end

  @impl true
  def handle_info(other, menu = %Menu{mod: mod}) do
    {:noreply, menu} = mod.handle_info(other, menu)
    {:noreply, update_dom(menu)}
  end

  def menubar(pid) do
    GenServer.call(pid, :menubar)
  end

  def create_menu(pid) when is_atom(pid) do
    create_menu(Process.whereis(pid))
  end

  def create_menu(pid) do
    dom = Desktop.Env.await({:dom, pid})

    {menu, callbacks} =
      :wx.batch(fn ->
        menu = :wxMenu.new()
        callbacks = do_create_menu(pid, [menu], dom)
        {menu, callbacks}
      end)

    # Would like to to this synchronously, but this is running in the context of
    # :wxe_server and so :wxe_server is blocked to accept :connect() calls
    GenServer.cast(pid, {:update_callbacks, callbacks})
    menu
  end

  defp do_create_menubar_menues(pid, dom) when is_list(dom) do
    Enum.filter(dom, fn {tag, _attr, _content} -> tag == :menu end)
    |> Enum.map(fn {:menu, attr, content} ->
      menu = :wxMenu.new()
      callbacks = do_create_menu(pid, [menu], content)
      label = String.to_charlist(attr[:label] || "")
      {label, menu, callbacks}
    end)
  end

  defp do_update_menubar(menu = %Menu{menubar: menubar}, menues) do
    size = :wxMenuBar.getMenuCount(menubar)

    Enum.with_index(menues)
    |> Enum.map(fn {{label, menu, _callbacks}, pos} ->
      if pos < size do
        :wxMenuBar.replace(menubar, pos, menu, label)
      else
        :wxMenuBar.append(menubar, menu, label)
      end
    end)

    if length(menues) < size do
      for pos <- length(menues)..(size - 1), do: :wxMenuBar.remove(menubar, pos)
    end

    bindings =
      Enum.map(menues, fn {_label, _menu, callback} -> callback end)
      |> update_callbacks()

    %Menu{menu | bindings: bindings}
  end

  defp do_create_menu(pid, menues, dom) when is_list(dom) do
    dom = OS.invert_menu(dom)

    Enum.map(dom, fn e -> do_create_menu(pid, menues, e) end)
  end

  defp do_create_menu(pid, menues, dom) when is_tuple(dom) do
    case dom do
      {:hr, _attr, _content} ->
        :wxMenu.appendSeparator(hd(menues))

      {:item, attr, content} ->
        kind =
          case attr[:type] do
            "checkbox" -> Wx.wxITEM_CHECK()
            "separator" -> Wx.wxITEM_SEPARATOR()
            "radio" -> Wx.wxITEM_RADIO()
            _other -> Wx.wxITEM_NORMAL()
          end

        item = :wxMenuItem.new(id: Wx.wxID_ANY(), text: List.flatten(content), kind: kind)
        id = :wxMenuItem.getId(item)
        :wxMenu.append(hd(menues), item)

        if attr[:checked] != nil do
          :wxMenuItem.check(item, check: is_true(attr[:checked]))
        end

        if is_true(attr[:disabled]) do
          :wxMenu.enable(hd(menues), id, false)
        end

        if attr[:onclick] != nil do
          event_src = if OS.windows?(), do: List.last(menues), else: hd(menues)

          # Wx keeps track of the calling process ot the connect call, so we collect
          # all connect's and call them from the GenServer itself.
          {:connect, {event_src, id, attr[:onclick]}}
        end

      {:menu, attr, content} ->
        menu = :wxMenu.new()
        ret = do_create_menu(pid, [menu | menues], content)
        :wxMenu.append(hd(menues), Wx.wxID_ANY(), String.to_charlist(attr[:label] || ""), menu)
        ret
    end
  end

  def is_true(value) do
    value != nil and value != "false" and value != "0"
  end

  defp unicode(string) do
    :unicode.characters_to_list(string)
  end

  def escape(string) do
    unicode(string)
    |> :xmerl_lib.export_text()
    |> List.to_string()
  end

  for tag <- [:xmlElement, :xmlAttribute, :xmlText] do
    Record.defrecordp(tag, Record.extract(tag, from_lib: "xmerl/include/xmerl.hrl"))
  end

  defp parse(string) do
    string = unicode(string)

    try do
      {xml, []} = :xmerl_scan.string(string, encoding: :ref)

      # 'simple-form' is what xmerl documentation calls a tree of tuples:
      # {tag, attributes = %{key => value}, content = []}
      simple_form(xmlElement(xml, :content))
    catch
      :exit, error ->
        Logger.error("Failed to parse document #{inspect(error)}")

        []
    end
  end

  defp simple_form([]) do
    []
  end

  defp simple_form([element | rest]) do
    case simple_form(element) do
      :skip -> simple_form(rest)
      ret -> [ret | simple_form(rest)]
    end
  end

  defp simple_form(xmlElement(name: name, attributes: attr, content: children)) do
    attr =
      Enum.map(attr, fn xmlAttribute(name: tname, value: value) ->
        {tname, List.to_string(value)}
      end)
      |> Map.new()

    {name, attr, simple_form(children)}
  end

  defp simple_form(xmlText(value: value)) do
    List.to_string(value)
    |> String.trim()
    |> case do
      "" -> :skip
      other -> other
    end
  end
end
