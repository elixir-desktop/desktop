defmodule Desktop.Menu.Adapter.Wx do
  @moduledoc """
  WX Menu Adapter that creates menus and icons using :wx
  """
  alias Desktop.{Wx, OS}
  alias Desktop.Wx.TaskBarIcon

  require Record
  require Logger

  @wxMenuItemSeparator Wx.wxITEM_SEPARATOR()

  defstruct [
    :menu_pid,
    :env,
    :bindings,
    :old_bindings,
    :menubar,
    :menubar_opts,
    :taskbar_icon,
    :wx_menu,
    :skip_tb_popup?
  ]

  @type t() :: %__MODULE__{
          menu_pid: pid() | nil,
          env: any(),
          menubar: any(),
          menubar_opts: any(),
          taskbar_icon: TaskBarIcon.t() | nil,
          skip_tb_popup?: boolean(),
          wx_menu: any(),
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
      taskbar_icon: nil,
      skip_tb_popup?: false,
      wx_menu: nil,
      bindings: %{},
      old_bindings: %{}
    }
  end

  def create(adapter = %__MODULE__{env: env, menubar_opts: menubar_opts}, dom) do
    :wx.set_env(env)

    create_menubar(adapter, menubar_opts, dom)
  end

  # defp create_menubar(adapter = %__MODULE__{}, {:taskbar, icon}, dom) do
  #   adapter =
  #     %{adapter | menubar: :wxMenuBar.new(), call_taskbar_popupmenu: false}
  #     |> create_menu(dom)

  #   create_popup = fn ->
  #     create_popup_menu(adapter)
  #   end

  #   {handle_popup?, taskbar_icon} = create_taskbar_icon(create_popup, icon)
  #   %{adapter | taskbar_icon: taskbar_icon, handle_tb_popup?: handle_popup?}
  # end

  defp create_menubar(adapter = %__MODULE__{}, {:taskbar, icon}, dom) do
    menubar = :wxMenuBar.new()

    adapter =
      %{adapter | menubar: menubar}
      |> create_menu(dom)

    create_popup = fn ->
      IO.inspect(menubar, label: "CREATE POPUP CALLBACK")
      create_popup_menu(adapter)
    end

    IO.inspect("CREATING TASKBARICON")

    taskbar_icon =
      create_taskbar_icon(create_popup, icon)
      |> IO.inspect(label: "CREATED TaskBarIcon")

    %{adapter | taskbar_icon: taskbar_icon}
  end

  defp create_menubar(adapter = %__MODULE__{}, wx_ref, _dom) do
    if :wxMenuBar == :wx.getObjectType(wx_ref) do
      %{adapter | menubar: wx_ref, taskbar_icon: nil}
    else
      %{adapter | menubar: nil, taskbar_icon: nil}
    end
  end

  defp create_taskbar_icon(fn_create_popup, icon) do
    with {:ok, taskbar_icon = %TaskBarIcon{wx_taskbar_icon: wx_taskbar_icon}} <-
           TaskBarIcon.create(fn_create_popup) do
      TaskBarIcon.connect(taskbar_icon)
      TaskBarIcon.setIcon(taskbar_icon, icon)

      if OS.type() == Windows do
        # This links the taskbar icon and the application itself on Windows
        if Code.ensure_loaded?(:wxNotificationMessage) &&
             Kernel.function_exported?(:wxNotificationMessage, :useTaskBarIcon, 1) do
          :wxNotificationMessage.useTaskBarIcon(wx_taskbar_icon)
        end
      end

      taskbar_icon
    else
      error ->
        IO.inspect(error, label: "CREATE TASKBAR ICON ERROR")
        Logger.debug(error)
        nil
    end
  end

  # defp create_taskbar_icon(fn_popup, icon) do
  #   skip_tb_popup? = false
  #   # Proper OTP24 release
  #   taskbar_icon =
  #     if OS.type() == MacOS do
  #       if is_module?(:wxWebView) do
  #         :wxTaskBarIcon.new(createPopupMenu: fn_popup)
  #       else
  #         # Pre-OTP24 custom version for backwards compat.
  #         try do
  #           :wxTaskBarIcon.new(fn_popup)
  #         catch
  #           :error, :function_clause ->
  #             Logger.error("No MacOS compatible :wxTaskBarIcon found! Please use at least OTP24")

  #             nil
  #         end
  #       end
  #     end

  #   taskbar_icon =
  #     if taskbar_icon == nil do
  #       # This works better under Linux and Windows (uses either mouse button for the menu)
  #       # but this doesn't work on MacOS at all :-(
  #       taskbar_icon = :wxTaskBarIcon.new(createPopupMenu: fn_popup)
  #       # taskbar_icon = :wxTaskBarIcon.new()
  #       :wxTaskBarIcon.connect(taskbar_icon, :taskbar_left_down, skip: true)
  #       :wxTaskBarIcon.connect(taskbar_icon, :taskbar_right_down, skip: true)
  #       taskbar_icon
  #     else
  #       taskbar_icon
  #     end

  #   true = :wxTaskBarIcon.setIcon(taskbar_icon, icon)

  #   if OS.type() == Windows do
  #     # This links the taskbar icon and the application itself on Windows
  #     call(:wxNotificationMessage, :useTaskBarIcon, [taskbar_icon])
  #   end

  #   OnCrash.call(fn ->
  #     :wx.set_env(Desktop.Env.wx_env())
  #     :wxTaskBarIcon.removeIcon(taskbar_icon)
  #     :wxTaskBarIcon.destroy(taskbar_icon)
  #   end)

  #   {skip_tb_popup?, taskbar_icon}
  # end

  defp create_popup_menu(%__MODULE__{menubar: nil}) do
    nil
  end

  defp create_popup_menu(adapter = %__MODULE__{menubar: menubar}) do
    IO.inspect(menubar, label: "CREATE POPUP MENU")

    spawn_link(fn ->
      adapter = GenServer.call(adapter.menu_pid, :get_adapter)
      IO.inspect(Map.take(adapter, [:bindings, :old_bindings]), label: "ADAPTER BINDINGS")
    end)

    IO.inspect(:wxMenuBar.getMenuCount(menubar), label: "NUMBER OF MENUS IS MENUBAR")

    menubar_menu = :wxMenuBar.getMenu(menubar, 0)

    # {menu, items} = wx_clone_menu(menubar_menu)
    # IO.inspect({menubar_menu, self()}, label: "event_src [menubar_menu]")

    # # _bindings =
    # #   items
    # #   |> Enum.map(fn item ->
    # #     id = :wxMenuItem.getId(item)
    # #     IO.inspect({id, item}, label: "Do connect")

    # #     :wxMenu.connect(
    # #       menu,
    # #       :command_menu_selected,
    # #       id: id,
    # #       callback: fn a, b ->
    # #         IO.inspect({id, a, b}, label: "GOT CALLBACK")
    # #       end
    # #     )
    # #   end)
    # :wxMenu.connect(
    #   menu,
    #   :menu_open,
    #   callback: fn menuId, mmenu ->
    #     IO.inspect({menuId, mmenu}, label: "GOT MENU OPEN")
    #   end
    # )

    # :wxMenu.connect(
    #   menu,
    #   :menu_close,
    #   callback: fn menuId, mmenu ->
    #     IO.inspect({menuId, mmenu}, label: "GOT MENU CLOSE")
    #   end
    # )

    # :wxMenu.connect(
    #   menu,
    #   :command_menu_selected,
    #   callback: fn a, b ->
    #     IO.inspect({a, b}, label: "GOT CALLBACK (any id)")
    #   end
    # )

    :wxMenuBar.remove(menubar, 0)
    menu = menubar_menu

    :wxMenu.connect(
      menu,
      :menu_close,
      callback: fn menuId, mmenu ->
        IO.inspect({menuId, mmenu}, label: "GOT MENU CLOSE")
      end
    )

    menu
  end

  # defp wx_clone_menu(menu) do
  #   if :wx.getObjectType(menu) == :wxMenu do
  #     IO.inspect({menu, self()}, label: "wx_clone_menu:")
  #     # Clone the menu.
  #     wx_menu = :wxMenu.new()
  #     :wxMenu.setTitle(wx_menu, :wxMenu.getTitle(menu))

  #     menu
  #     |> :wxMenu.getMenuItems()
  #     |> Enum.reduce({wx_menu, []}, &wx_clone_menu_item(&2, &1))
  #   else
  #     {:wx.null(), []}
  #   end
  # end

  # defp wx_clone_menu_item({menu, prev_items}, src) do
  #   {menu, items} = wx_clone_menu_item(menu, src)
  #   {menu, prev_items ++ items}
  # end

  # defp wx_clone_menu_item(menu, src) do
  #   id = :wxMenuItem.getId(src)

  #   item =
  #     case :wxMenuItem.getKind(src) do
  #       kind = @wxMenuItemSeparator ->
  #         :wxMenuItem.new(id: id, text: "", kind: kind)

  #       kind ->
  #         :wxMenuItem.new(
  #           # id: Wx.wxID_ANY(),
  #           id: id,
  #           text: :wxMenuItem.getText(src),
  #           kind: kind
  #         )
  #     end

  #   id = :wxMenuItem.getId(item)

  #   :wxMenu.append(menu, item)

  #   if :wxMenuItem.isCheckable(src) do
  #     :wxMenuItem.check(item, check: :wxMenuItem.isChecked(src))
  #   end

  #   :wxMenu.enable(menu, id, :wxMenuItem.isEnabled(item))

  #   submenu = :wxMenuItem.getSubMenu(src)

  #   if :wx.is_null(submenu) == false do
  #     {submenu, items} = wx_clone_menu(submenu)
  #     :wxMenuItem.setSubMenu(item, submenu)
  #     {menu, [item | items]}
  #   else
  #     {menu, [item]}
  #   end
  # end

  def update_dom(adapter, {:menubar, _, children}) do
    update_dom(adapter, children)
  end

  def update_dom(adapter, {:menu, attrs, children}) do
    attrs =
      if Map.has_key?(attrs, :label) do
        attrs
      else
        Map.put(attrs, :label, "Default")
      end

    update_dom(adapter, [{:menu, attrs, children}])
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

  def popup_menu(adapter = %__MODULE__{}, dom) do
    case create_menu(adapter, dom) do
      adapter = %{taskbar_icon: taskbar_icon}
      when not is_nil(taskbar_icon) ->
        # :wxTaskBarIcon.popupMenu(taskbar_icon, create_popup_menu(adapter))
        IO.inspect(adapter.menubar, label: "ADAPTER POPUP MENU")
        IO.inspect(taskbar_icon)
        TaskBarIcon.popupMenu(taskbar_icon)
        adapter

      adapter ->
        adapter
    end
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

  def set_icon(adapter = %__MODULE__{taskbar_icon: nil}, nil) do
    {:ok, adapter}
  end

  def set_icon(adapter = %__MODULE__{taskbar_icon: taskbar_icon}, nil) do
    # :wxTaskBarIcon.removeIcon(taskbar_icon)
    TaskBarIcon.removeIcon(taskbar_icon)
    {:ok, adapter}
  end

  def set_icon(adapter = %__MODULE__{taskbar_icon: nil}, _icon) do
    {:ok, adapter}
  end

  def set_icon(adapter = %__MODULE__{taskbar_icon: taskbar_icon}, icon) do
    # :wxTaskBarIcon.setIcon(taskbar_icon, icon)
    TaskBarIcon.setIcon(taskbar_icon, icon)
    {:ok, adapter}
  end

  def handle_info(
        wx(id: id, event: wxCommand(type: :command_menu_selected)),
        adapter = %{bindings: bindings, old_bindings: old, menu_pid: menu_pid},
        _dom
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

  def handle_info(
        wx(event: {:wxTaskBarIcon, :taskbar_left_down}),
        adapter,
        dom
      ) do
    {:noreply, popup_menu(adapter, dom)}
  end

  def handle_info(
        wx(event: {:wxTaskBarIcon, :taskbar_right_down}),
        adapter,
        dom
      ) do
    {:noreply, popup_menu(adapter, dom)}
  end

  def handle_info(event = wx(), adapter, _) do
    Logger.warning("Desktop.Menu received unexpected wx message #{inspect({event, adapter})}")
    {:noreply, adapter}
  end

  # Private functions

  defp create_menu(adapter = %__MODULE__{}, dom) do
    :wx.set_env(Desktop.Env.wx_env())

    {_wx_menu, callbacks} =
      :wx.batch(fn ->
        wx_menu = :wxMenu.new()
        callbacks = do_create_menu([wx_menu], dom, OS.windows?())
        {wx_menu, callbacks}
      end)

    # Would like to do this synchronously, but this is running in the context of
    # :wxe_server and so :wxe_server is blocked to accept :connect() calls
    update_callbacks(adapter, callbacks)
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
          IO.inspect({event_src, self()}, label: "event_src")
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
            IO.inspect({id, event_src}, label: "make wxmenu connect")

            :wxMenu.connect(
              event_src,
              :command_menu_selected,
              id: id
            )

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

  defp is_true(value) do
    value != nil and value != "false" and value != "0"
  end

  # defp is_module?(module) do
  #   Code.ensure_compiled(module) == {:module, module}
  # end

  # defp call(module, method, args) do
  #   if Kernel.function_exported?(module, method, length(args)) do
  #     apply(module, method, args)
  #   end
  # end
end
