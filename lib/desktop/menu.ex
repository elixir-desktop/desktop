defmodule Desktop.Menu do
  alias Desktop.{Menu, Wx, OS}
  require Record
  require Logger
  use GenServer
  defstruct [:assigns, :dom, :mod, :bindings, :menubar, :taskbar]

  @type t() :: %__MODULE__{
          assigns: %{},
          dom: [],
          mod: Atom.t(),
          bindings: %{}
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

  def init([mod, wx_env, bar]) do
    :wx.set_env(wx_env)

    menu =
      case :wx.getObjectType(bar) do
        :wxTaskBarIcon -> %Menu{mod: mod, assigns: %{}, bindings: %{}, taskbar: bar}
        :wxMenuBar -> %Menu{mod: mod, assigns: %{}, bindings: %{}, menubar: bar}
      end

    {:ok, menu} = mod.mount(menu)
    {:ok, update_dom(menu)}
  end

  defp update_dom(menu = %Menu{mod: mod, assigns: assigns, menubar: menubar, dom: dom}) do
    new_dom = mod.render(assigns) |> parse()

    if menubar != nil and dom != new_dom do
      create_menu_bar(self(), menubar, new_dom)
    end

    %Menu{menu | dom: new_dom}
  end

  def handle_call(:dom, _from, menu = %Menu{dom: dom}) do
    {:reply, dom, menu}
  end

  def handle_cast({:connect, event_src, id, onclick}, menu = %Menu{bindings: bindings}) do
    :wxMenu.connect(event_src, :command_menu_selected, id: id)
    {:noreply, %Menu{menu | bindings: Map.put(bindings, id, onclick)}}
  end

  for tag <- [:wx, :wxCommand] do
    Record.defrecordp(tag, Record.extract(tag, from_lib: "wx/include/wx.hrl"))
  end

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

  def handle_info(event = wx(), menu) do
    Logger.warning("Desktop.Menu received unexpected wx message", [event, menu])
    {:noreply, menu}
  end

  def handle_info(other, menu = %Menu{mod: mod}) do
    {:noreply, menu} = mod.handle_info(other, menu)
    {:noreply, update_dom(menu)}
  end

  def create_menu_bar(pid, menubar, dom \\ nil) do
    dom = dom || GenServer.call(pid, :dom)

    :wx.batch(fn ->
      do_create_menu_bar(pid, menubar, dom)
    end)
  end

  def create_menu(pid) do
    dom = GenServer.call(pid, :dom)

    :wx.batch(fn ->
      menu = :wxMenu.new()
      do_create_menu(pid, [menu], dom)
      menu
    end)
  end

  defp do_create_menu_bar(pid, menubar, dom) when is_list(dom) do
    size = :wxMenuBar.getMenuCount(menubar)
    dom = Enum.filter(dom, fn {tag, _attr, _content} -> tag == :menu end)

    Enum.with_index(dom)
    |> Enum.each(fn {{:menu, attr, content}, pos} ->
      menu = :wxMenu.new()
      do_create_menu(pid, [menu], content)
      label = String.to_charlist(attr[:label] || "")

      if pos < size do
        :wxMenuBar.replace(menubar, pos, menu, label)
      else
        :wxMenuBar.append(menubar, menu, label)
      end
    end)

    if length(dom) < size do
      for pos <- length(dom)..(size - 1), do: :wxMenuBar.remove(menubar, pos)
    end

    menubar
  end

  defp do_create_menu(pid, menues, dom) when is_list(dom) do
    dom = OS.invert_menu(dom)

    Enum.each(dom, fn e ->
      do_create_menu(pid, menues, e)
    end)
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

        if attr[:onclick] != nil do
          event_src = if OS.windows?(), do: List.last(menues), else: hd(menues)

          # Wx keeps track of the calling process ot the connect call, so we ensure
          # all connect's are called from the GenServer itself.
          GenServer.cast(pid, {:connect, event_src, id, attr[:onclick]})
        end

        if is_true(attr[:disabled]) do
          :wxMenu.enable(hd(menues), id, false)
        end

      {:menu, attr, content} ->
        menu = :wxMenu.new()
        do_create_menu(pid, [menu | menues], content)
        :wxMenu.append(hd(menues), Wx.wxID_ANY(), String.to_charlist(attr[:label] || ""), menu)
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
