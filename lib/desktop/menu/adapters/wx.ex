defmodule Desktop.Menu.Adapter.Wx do
  alias Desktop.{Wx, OS, Fallback}
  require Record
  require Logger
  # use GenServer
  defstruct [:mod, :env, :bindings, :old_bindings, :menubar, :pid, :loaded]

  @type t() :: %__MODULE__{
          bindings: %{},
          # We're keeping one generation of bindings because the menubaricon will usually have
          # two (2) active generations. The currently displayed one and the newly generated one.
          old_bindings: %{},
          pid: pid()
        }

  for tag <- [:wx, :wxCommand] do
    Record.defrecordp(tag, Record.extract(tag, from_lib: "wx/include/wx.hrl"))
  end

  def new(env, module) do
    %__MODULE__{
      mod: module,
      bindings: %{},
      old_bindings: %{},
      env: env,
      pid: nil
    }
  end

  def create(%__MODULE__{env: env} = menu, dom, opts) do
    :wx.set_env(env)

    case opts do
      {:taskbar, icon} ->
        create_popup = fn -> create_popup_menu(menu, dom) end
        %{menu | menubar: Fallback.taskbaricon_new_wx(create_popup, icon)}

      wx_ref ->
        :wxMenuBar = :wx.getObjectType(wx_ref)
        %{menu | menubar: wx_ref}
    end
  end

  defp create_popup_menu(%__MODULE__{} = menu, dom) do
    menu(menu, dom)
  end

  def menu(%__MODULE__{} = menu, dom) do
    # This will create the menu for next round
    spawn(fn -> create_menu(menu, dom) end)
  end

  def update_dom(%__MODULE__{menubar: menubar} = menu, dom) do
    # if dom != new_dom do
    if menubar != nil do
      :wx.set_env(Desktop.Env.wx_env())
      menues = :wx.batch(fn -> do_create_menubar_menues(dom) end)
      update_menubar(menu, menues)
    else
      create_menu(menu, dom)
    end
  end

  def popup_menu(%__MODULE__{menubar: bar} = menu, dom) do
    :wxTaskBarIcon.popupMenu(bar, create_popup_menu(menu, dom))
    menu
  end

  defp create_menu(%__MODULE__{} = menu, dom) do
    # dom = Desktop.Env.await({:dom, pid})
    :wx.set_env(Desktop.Env.wx_env())

    {wx_menu, callbacks} =
      :wx.batch(fn ->
        wx_menu = :wxMenu.new()
        callbacks = do_create_menu([wx_menu], dom, OS.windows?())
        {wx_menu, callbacks}
      end)

    # Would like to do this synchronously, but this is running in the context of
    # :wxe_server and so :wxe_server is blocked to accept :connect() calls
    menu =
      :wx.batch(fn ->
        update_callbacks(menu, callbacks)
      end)

    case menu do
      nil -> :ok
      _menu -> :wxMenu.destroy(wx_menu)
    end

    menu
  end

  defp update_callbacks(menu = %__MODULE__{bindings: bindings, old_bindings: old}, callbacks) do
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

  defp update_menubar(%__MODULE__{menubar: _bar, loaded: _loaded} = menu, menues) do
    menu =
      :wx.batch(fn ->
        do_update_menubar(menu, menues)
      end)

    # case loaded do
    #   {:from, from} -> GenServer.reply(from, bar)
    #   _ -> :ok
    # end
    %{menu | loaded: true}
  end

  defp do_update_menubar(menu = %__MODULE__{menubar: menubar}, menues) do
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

    callbacks = Enum.map(menues, fn {_label, _menu, callback} -> callback end)
    update_callbacks(menu, callbacks)
  end

  defp do_create_menubar_menues(dom) when is_list(dom) do
    Enum.filter(dom, fn {tag, _attr, _content} -> tag == :menu end)
    |> Enum.map(fn {:menu, attr, content} ->
      menu = :wxMenu.new()
      callbacks = do_create_menu([menu], content, false)
      label = String.to_charlist(attr[:label] || "")
      {label, menu, callbacks}
    end)
  end

  defp do_create_menu(menues, dom, invert) when is_list(dom) do
    if(invert, do: Enum.reverse(dom), else: dom)
    |> Enum.map(fn e -> do_create_menu(menues, e, invert) end)
  end

  defp do_create_menu(menues, dom, invert) when is_tuple(dom) do
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
        ret = do_create_menu([menu | menues], content, invert)
        :wxMenu.append(hd(menues), Wx.wxID_ANY(), String.to_charlist(attr[:label] || ""), menu)
        ret
    end
  end

  def is_true(value) do
    value != nil and value != "false" and value != "0"
  end
end
