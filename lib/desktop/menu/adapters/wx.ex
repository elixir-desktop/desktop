defmodule Desktop.Menu.Adapter.Wx do
  alias Desktop.{Wx, OS, Fallback}

  require Record
  require Logger

  defstruct [:menu_pid, :env, :bindings, :old_bindings, :menubar, :menubar_opts]

  @type t() :: %__MODULE__{
          menu_pid: pid() | nil,
          env: any(),
          menubar: any(),
          menubar_opts: any(),
          bindings: %{},
          # We're keeping one generation of bindings because the menubaricon will usually have
          # two (2) active generations. The currently displayed one and the newly generated one.
          old_bindings: %{}
        }

  for tag <- [:wx, :wxCommand] do
    Record.defrecordp(tag, Record.extract(tag, from_lib: "wx/include/wx.hrl"))
  end

  def new(opts) do
    %__MODULE__{
      env: Keyword.get(opts, :env),
      menu_pid: Keyword.get(opts, :menu_pid),
      menubar: nil,
      menubar_opts: Keyword.get(opts, :wx),
      bindings: %{},
      old_bindings: %{}
    }
  end

  def create(adapter = %__MODULE__{env: env, menubar_opts: menubar_opts}, dom) do
    :wx.set_env(env)

    menubar =
      case menubar_opts do
        {:taskbar, icon} ->
          create_popup = fn -> create_popup_menu(adapter, dom) end
          Fallback.taskbaricon_new_wx(create_popup, icon)

        wx_ref ->
          if :wxMenuBar == :wx.getObjectType(wx_ref) do
            wx_ref
          else
            nil
          end
      end

    %{adapter | menubar: menubar}
  end

  defp create_popup_menu(adapter = %__MODULE__{}, dom) do
    menu(adapter, dom)
  end

  def menu(adapter = %__MODULE__{}, dom) do
    # This will create the menu for next round
    spawn(fn -> create_menu(adapter, dom) end)

    # Desktop.Env.pop({:menu, server})
  end

  def update_dom(adapter = %__MODULE__{menubar: menubar}, dom) do
    # Desktop.Env.put({:dom, server}, dom)

    if menubar != nil do
      :wx.set_env(Desktop.Env.wx_env())
      menues = :wx.batch(fn -> do_create_menubar_menues(dom) end)
      update_menubar(adapter, menues)
    else
      create_menu(adapter, dom)
    end
  end

  def popup_menu(adapter = %__MODULE__{menubar: bar}, dom) do
    :wxTaskBarIcon.popupMenu(bar, create_popup_menu(adapter, dom))
    adapter
  end

  def get_icon(%__MODULE__{menubar: nil}) do
    nil
  end

  def get_icon(%__MODULE__{menubar: _}) do
    # nil
  end

  def set_icon(%__MODULE__{menubar: nil}, _) do
    {:error, "Cannot set icon on `nil` taskbar"}
  end

  def set_icon(adapter = %__MODULE__{menubar: bar}, nil) do
    :wxTaskBarIcon.removeIcon(bar)
    {:ok, adapter}
  end

  def set_icon(adapter = %__MODULE__{menubar: bar}, icon) do
    :wxTaskBarIcon.setIcon(bar, icon)
    {:ok, adapter}
  end

  def handle_info(
        wx(id: id, event: wxCommand(type: :command_menu_selected)),
        adapter = %{bindings: bindings, old_bindings: old, menu_pid: menu_pid}
      ) do
    Map.merge(old, bindings)
    |> Map.get(id)
    |> case do
      nil ->
        Logger.warning("Desktop.Menu unbound message id #{inspect(id)}")

      name ->
        spawn_link(Desktop.Menu, :trigger_event, [menu_pid, name])
    end

    {:noreply, adapter}
  end

  def handle_info(event = wx(), adapter) do
    Logger.warning("Desktop.Menu received unexpected wx message #{inspect({event, adapter})}")
    {:noreply, adapter}
  end

  # Private functions

  defp create_menu(adapter = %__MODULE__{}, dom) do
    # dom = Desktop.Env.await({:dom, server})

    :wx.set_env(Desktop.Env.wx_env())

    {wx_menu, callbacks} =
      :wx.batch(fn ->
        wx_menu = :wxMenu.new()
        callbacks = do_create_menu([wx_menu], dom, OS.windows?())
        {wx_menu, callbacks}
      end)

    # Would like to do this synchronously, but this is running in the context of
    # :wxe_server and so :wxe_server is blocked to accept :connect() calls

    adapter = update_callbacks(adapter, callbacks)

    case adapter do
      nil -> :ok
      _adapter -> :wxMenu.destroy(wx_menu)
    end

    adapter
  end

  defp update_callbacks(
         adapter = %__MODULE__{},
         callbacks
       ) do
    :wx.batch(fn ->
      do_update_callbacks(adapter, callbacks)
    end)
  end

  defp update_menubar(adapter = %__MODULE__{menubar: _bar}, menues) do
    adapter =
      :wx.batch(fn ->
        do_update_menubar(adapter, menues)
      end)

    # case loaded do
    #   {:from, from} -> GenServer.reply(from, bar)
    #   _ -> :ok
    # end
    # %{adapter | loaded: true}
    adapter
  end

  defp do_update_menubar(adapter = %__MODULE__{menubar: menubar}, menues) do
    size = :wxMenuBar.getMenuCount(menubar)

    Enum.with_index(menues)
    |> Enum.each(fn {{label, adapter, _callbacks}, pos} ->
      if pos < size do
        :wxMenuBar.replace(menubar, pos, adapter, label)
      else
        :wxMenuBar.append(menubar, adapter, label)
      end
    end)

    if length(menues) < size do
      for pos <- length(menues)..(size - 1), do: :wxMenuBar.remove(menubar, pos)
    end

    callbacks = Enum.map(menues, fn {_label, _menu, callback} -> callback end)
    update_callbacks(adapter, callbacks)
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

  defp do_update_callbacks(
         adapter = %__MODULE__{bindings: bindings, old_bindings: old},
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

    %{adapter | bindings: bindings, old_bindings: old}
  end

  def is_true(value) do
    value != nil and value != "false" and value != "0"
  end
end
