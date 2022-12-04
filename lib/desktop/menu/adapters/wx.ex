defmodule Desktop.Menu.Adapter.Wx do
  @moduledoc false
  alias Desktop.{Wx, OS}
  alias Desktop.Wx.TaskBarIcon

  require Record
  require Logger

  defstruct [
    :menu_pid,
    :env,
    :menubar,
    :menubar_opts,
    :taskbar_icon
  ]

  @type t() :: %__MODULE__{
          menu_pid: pid() | nil,
          env: any(),
          menubar: any(),
          menubar_opts: any(),
          taskbar_icon: TaskBarIcon.t() | nil
        }

  for tag <- [:wx, :wxCommand, :wxMenu] do
    Record.defrecordp(tag, Record.extract(tag, from_lib: "wx/include/wx.hrl"))
  end

  def new(opts) do
    %__MODULE__{
      env: Keyword.get(opts, :env),
      menu_pid: Keyword.get(opts, :menu_pid),
      menubar: nil,
      menubar_opts: Keyword.get(opts, :wx),
      taskbar_icon: nil
    }
  end

  def create(adapter = %__MODULE__{env: env, menubar_opts: menubar_opts}, dom) do
    :wx.set_env(env)

    create_menubar(adapter, menubar_opts, dom)
  end

  def update_dom(adapter = %__MODULE__{}, dom) do
    create_menu(adapter, dom)
  end

  def popup_menu(adapter = %__MODULE__{}) do
    do_popup_menu(adapter, :taskbar_click)
  end

  def recreate_menu(adapter = %__MODULE__{}, dom) do
    create_menu(adapter, dom)
  end

  def menubar(%__MODULE__{menubar: menubar}) do
    menubar
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
    TaskBarIcon.remove_icon(taskbar_icon)
    {:ok, adapter}
  end

  def set_icon(adapter = %__MODULE__{taskbar_icon: nil}, _icon) do
    {:ok, adapter}
  end

  def set_icon(adapter = %__MODULE__{taskbar_icon: taskbar_icon}, icon) do
    TaskBarIcon.set_icon(taskbar_icon, icon)
    {:ok, adapter}
  end

  def handle_info(
        wx(id: _id, event: wxCommand(type: :command_menu_selected), userData: user_data),
        adapter = %{menu_pid: menu_pid}
      ) do
    Desktop.Menu.trigger_event(menu_pid, user_data)
    {:noreply, adapter}
  end

  def handle_info(
        wx(id: _id, event: wxMenu(type: :menu_close), userData: _user_data),
        adapter = %{}
      ) do
    {:noreply, adapter}
  end

  def handle_info(
        wx(event: {:wxTaskBarIcon, :taskbar_left_down}),
        adapter
      ) do
    {:noreply, do_popup_menu(adapter, :taskbar_left_down)}
  end

  def handle_info(
        wx(event: {:wxTaskBarIcon, :taskbar_right_down}),
        adapter
      ) do
    {:noreply, do_popup_menu(adapter, :taskbar_right_down)}
  end

  def handle_info(event = wx(), adapter) do
    Logger.warning("Desktop.Menu received unexpected wx message #{inspect({event, adapter})}")
    {:noreply, adapter}
  end

  # Private functions

  defp do_popup_menu(adapter = %__MODULE__{taskbar_icon: taskbar_icon}, event) do
    TaskBarIcon.popup_menu(taskbar_icon, event)
    adapter
  end

  defp create_menubar(adapter = %__MODULE__{}, {:taskbar, icon}, dom) do
    menubar = :wxMenuBar.new()

    adapter =
      %{adapter | menubar: menubar}
      |> create_menu(dom)

    create_popup = fn ->
      create_popup_menu(adapter)
    end

    taskbar_icon = create_taskbar_icon(create_popup, icon)
    %{adapter | taskbar_icon: taskbar_icon}
  end

  defp create_menubar(adapter = %__MODULE__{}, wx_ref, dom) do
    if :wxMenuBar == :wx.getObjectType(wx_ref) do
      %{adapter | menubar: wx_ref, taskbar_icon: nil}
      |> create_menu(dom)
    else
      %{adapter | menubar: nil, taskbar_icon: nil}
    end
  end

  defp create_taskbar_icon(fn_create_popup, icon) do
    case TaskBarIcon.create(fn_create_popup) do
      {:ok, taskbar_icon = %TaskBarIcon{wx_taskbar_icon: wx_taskbar_icon}} ->
        TaskBarIcon.connect(taskbar_icon)
        TaskBarIcon.set_icon(taskbar_icon, icon)

        if OS.type() == Windows do
          # This links the taskbar icon and the application itself on Windows
          if Code.ensure_loaded?(:wxNotificationMessage) &&
               Kernel.function_exported?(:wxNotificationMessage, :useTaskBarIcon, 1) do
            :wxNotificationMessage.useTaskBarIcon(wx_taskbar_icon)
          end
        end

        taskbar_icon

      error ->
        Logger.warning("Failed to create TaskBar Icon: #{inspect(error)}")
        nil
    end
  end

  defp create_popup_menu(%__MODULE__{menubar: nil}) do
    :wx.null()
  end

  defp create_popup_menu(adapter = %__MODULE__{menubar: menubar}) do
    num_menus = :wxMenuBar.getMenuCount(menubar)

    if num_menus > 0 do
      menu = :wxMenuBar.remove(menubar, 0)

      # Because the menu is removed from the menubar
      # trigger a new menu create for next time the popup opens
      # outside of the Menu -> Adapter flow
      GenServer.cast(adapter.menu_pid, :recreate_menu)

      menu
    else
      :wx.null()
    end
  end

  defp create_menu(adapter, {:menubar, _, children}) do
    create_menu(adapter, children)
  end

  defp create_menu(adapter, {:menu, attrs, children}) do
    attrs =
      if Map.has_key?(attrs, :label) do
        attrs
      else
        Map.put(attrs, :label, "Default")
      end

    create_menu(adapter, [{:menu, attrs, children}])
  end

  defp create_menu(adapter = %__MODULE__{menubar: menubar}, dom) do
    :wx.set_env(Desktop.Env.wx_env())

    :wx.batch(fn ->
      menus = create_menu_items(nil, dom)

      # Enum.each(menus, fn {menu, _label} ->
      #   :wxMenu.connect(
      #     menu,
      #     :menu_close
      #   )
      # end)

      update_menubar(menubar, menus)
    end)

    adapter
  end

  defp update_menubar(adapter = %__MODULE__{menubar: menubar}, menus) do
    :wx.batch(fn ->
      update_menubar(menubar, menus)
    end)

    adapter
  end

  defp update_menubar(menubar, menus) do
    menubar
    |> do_reset_menubar()
    |> do_update_menubar(menus)
  end

  defp do_reset_menubar(menubar) do
    size = :wxMenuBar.getMenuCount(menubar)

    if size > 0 do
      menubar
      |> :wxMenuBar.remove(0)
      |> :wxMenu.destroy()

      do_reset_menubar(menubar)
    else
      menubar
    end
  end

  defp do_update_menubar(menubar, []) do
    menubar
  end

  defp do_update_menubar(menubar, [{menu, label} | menus]) do
    :wxMenuBar.append(menubar, menu, label)
    do_update_menubar(menubar, menus)
  end

  defp create_menu_items(_evt_handler, []) do
    []
  end

  defp create_menu_items(evt_handler, [{:menu, attr, content} | dom_elements]) do
    menu =
      Enum.reduce(content, :wxMenu.new(), fn dom_element, menu ->
        create_menu_item(menu, menu, dom_element)
      end)

    [{menu, attr[:label]} | create_menu_items(evt_handler, dom_elements)]
  end

  defp create_menu_items(evt_handler, [_ | dom_elements]) do
    create_menu_items(evt_handler, dom_elements)
  end

  defp create_menu_item(evt_handler, parent_menu, dom_element) do
    case dom_element do
      {:hr, _attr, _content} ->
        :wxMenu.appendSeparator(parent_menu)

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

        :wxMenu.append(parent_menu, item)

        if attr[:checked] != nil do
          :wxMenuItem.check(item, check: is_true(attr[:checked]))
        end

        if is_true(attr[:disabled]) do
          :wxMenu.enable(parent_menu, id, false)
        end

        if attr[:onclick] != nil do
          :wxMenu.connect(
            evt_handler,
            :command_menu_selected,
            id: id,
            userData: attr[:onclick]
          )
        end

      {:menu, attr, content} ->
        submenu = create_menu_item(evt_handler, :wxMenu.new(), content)

        :wxMenu.append(
          parent_menu,
          Wx.wxID_ANY(),
          String.to_charlist(attr[:label] || ""),
          submenu
        )

      [_ | _] = items ->
        ret =
          Enum.reduce(items, parent_menu, fn item, parent_menu ->
            create_menu_item(evt_handler, parent_menu, item)
            parent_menu
          end)

        ret

      _ ->
        nil
    end

    parent_menu
  end

  defp is_true(value) do
    value != nil and value != "false" and value != "0" and value != ""
  end
end
