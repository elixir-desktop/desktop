defmodule Desktop.Menu.Adapter.Json do
  @moduledoc false

  alias Desktop.Bridge.Protocol
  alias Desktop.{Wx, OS}
  alias Desktop.Wx.TaskBarIcon

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

  def new(opts) do
    %__MODULE__{
      env: Keyword.get(opts, :env),
      menu_pid: Keyword.get(opts, :menu_pid),
      menubar: nil,
      menubar_opts: Keyword.get(opts, :wx),
      taskbar_icon: nil
    }
  end

  def create(adapter = %__MODULE__{menubar_opts: menubar_opts}, dom) do
    create_menubar(adapter, menubar_opts, dom)
  end

  def update_dom(adapter, dom), do: create_menu(adapter, dom)
  def popup_menu(adapter), do: do_popup_menu(adapter, :taskbar_click)
  def recreate_menu(adapter, dom), do: create_menu(adapter, dom)
  def menubar(%__MODULE__{menubar: menubar}), do: menubar
  def get_icon(%__MODULE__{taskbar_icon: nil}), do: nil
  def get_icon(%__MODULE__{taskbar_icon: _}), do: nil

  def set_icon(%__MODULE__{menubar: nil}, _), do: {:error, "Cannot set icon on `nil` taskbar"}
  def set_icon(adapter = %__MODULE__{taskbar_icon: nil}, nil), do: {:ok, adapter}

  def set_icon(adapter = %__MODULE__{taskbar_icon: taskbar_icon}, nil) do
    TaskBarIcon.remove_icon(taskbar_icon)
    {:ok, adapter}
  end

  def set_icon(adapter = %__MODULE__{taskbar_icon: nil}, _icon), do: {:ok, adapter}

  def set_icon(adapter = %__MODULE__{taskbar_icon: taskbar_icon}, icon) do
    TaskBarIcon.set_icon(taskbar_icon, icon)
    {:ok, adapter}
  end

  def handle_info(_event, adapter), do: {:noreply, adapter}

  defp do_popup_menu(adapter = %__MODULE__{taskbar_icon: taskbar_icon}, event) do
    TaskBarIcon.popup_menu(taskbar_icon, event)
    adapter
  end

  defp create_menubar(adapter = %__MODULE__{}, {:taskbar, icon}, dom) do
    menubar = Protocol.new(:wxMenuBar, [])
    adapter = %{adapter | menubar: menubar}

    create_popup = fn -> create_popup_menu(adapter) end

    taskbar_icon =
      if OS.mobile?() do
        nil
      else
        case TaskBarIcon.create(create_popup) do
          {:ok, taskbar_icon} ->
            TaskBarIcon.set_icon(taskbar_icon, icon)
            taskbar_icon

          _ ->
            nil
        end
      end

    create_menu(adapter, dom)
    %{adapter | taskbar_icon: taskbar_icon}
  end

  defp create_menubar(adapter = %__MODULE__{}, wx_ref, dom) do
    adapter =
      if wx_ref do
        %{adapter | menubar: wx_ref, taskbar_icon: nil}
      else
        %{adapter | menubar: nil, taskbar_icon: nil}
      end

    create_menu(adapter, dom)
  end

  defp create_popup_menu(adapter = %__MODULE__{menubar: menubar}) do
    num_menus = Protocol.call(:wxMenuBar, :getMenuCount, [menubar]) || 0

    for _ <- 1..num_menus do
      menu = Protocol.call(:wxMenuBar, :remove, [menubar, 0])
      Protocol.destroy(:wxMenu, menu)
    end

    if adapter.menu_pid, do: GenServer.cast(adapter.menu_pid, :recreate_menu)
    menubar
  end

  defp create_menu(adapter = %__MODULE__{menubar: menubar}, dom) do
    menus = build_menus(dom)

    for {label, menu} <- menus do
      Protocol.call(:wxMenuBar, :append, [menubar, menu, label])
    end

    adapter
  end

  defp create_menu(adapter, _), do: adapter

  defp build_menus({:menubar, _, children}), do: build_menus(children)
  defp build_menus({:menu, attrs, children}), do: [{attrs[:label], build_menu_items(children)}]
  defp build_menus(list) when is_list(list), do: Enum.flat_map(list, &build_menus/1)
  defp build_menus(_), do: []

  defp build_menu_items(children) do
    Enum.reduce(children, Protocol.new(:wxMenu, []), fn
      {:hr, _, _}, menu ->
        Protocol.call(:wxMenu, :appendSeparator, [menu])
        menu

      {:item, attrs, content}, menu ->
        kind = item_kind(attrs[:type])

        item =
          Protocol.new(:wxMenuItem, id: Wx.wxID_ANY(), text: List.flatten(content), kind: kind)

        id = Protocol.call(:wxMenuItem, :getId, [item])
        Protocol.call(:wxMenu, :append, [menu, item])

        if attr_true?(attrs[:checked]) do
          Protocol.call(:wxMenuItem, :check, [item, [check: true]])
        end

        if attr_true?(attrs[:disabled]) do
          Protocol.call(:wxMenu, :enable, [menu, id, false])
        end

        if attrs[:onclick] do
          Protocol.connect(:wxMenu, menu, :command_menu_selected,
            userData: attrs[:onclick],
            id: id
          )
        end

        menu

      {:menu, attrs, content}, menu ->
        submenu = build_menu_items(content)
        Protocol.call(:wxMenu, :append, [menu, submenu, attrs[:label]])
        menu
    end)
  end

  defp item_kind("radio"), do: Wx.wxITEM_RADIO()
  defp item_kind("checkbox"), do: Wx.wxITEM_CHECK()
  defp item_kind(_), do: Wx.wxITEM_NORMAL()

  defp attr_true?(nil), do: false
  defp attr_true?(false), do: false
  defp attr_true?(0), do: false
  defp attr_true?(_), do: true
end
