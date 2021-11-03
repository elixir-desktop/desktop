defmodule Desktop.Menu.Adapter.DBus do
  @moduledoc """
  DBus Menu adapter that creates and handles
  menus and tray icons created over DBus.
  """

  require Logger

  alias ExSni.Icon
  alias ExSni.Icon.Info
  alias ExSni.Icon.Tooltip
  alias ExSni.Menu
  alias ExSni.Menu.Item
  alias Desktop.Pixmap

  defstruct [:menu_pid, :sni, :icon, :menubar]

  @type t() :: %__MODULE__{
          menu_pid: pid() | nil,
          sni: pid() | nil,
          icon: ExSni.Icon.t() | nil,
          menubar: ExSni.Menu.t() | nil
        }

  def new(opts) do
    %__MODULE__{
      menu_pid: Keyword.get(opts, :menu_pid),
      sni: Keyword.get(opts, :sni),
      icon: Keyword.get(opts, :icon),
      menubar: nil
    }
  end

  def create(adapter = %__MODULE__{sni: sni, menubar: menubar, icon: icon}, _dom) do
    if menubar != nil do
      ExSni.set_menu(sni, menubar)
    end

    if icon != nil do
      set_sni_icon(sni, icon)
    end

    adapter
  end

  def update_dom(adapter = %__MODULE__{menubar: _menubar, sni: sni}, dom) do
    IO.inspect("Update DOM start", label: "[#{System.os_time(:millisecond)}]")
    # ID 1 is reserved for visible separator
    # ID 2 is reserved for hidden separator
    {children, _} = create_menu_items(3, dom, adapter)
    IO.inspect("Created menu struct", label: "[#{System.os_time(:millisecond)}]")
    # root = %Item{id: 0, type: :root, children: children}
    root = Item.root(children)

    callbacks = [
      {:show,
       fn ->
         # ABOUT TO SHOW menubar
         # Return "needUpdate" = false
         false
       end}
    ]

    menubar = %Menu{root: root, callbacks: callbacks}
    IO.inspect("Updating menu over ExSni", label: "[#{System.os_time(:millisecond)}]")
    ExSni.update_menu(sni, nil, menubar)
    IO.inspect("Updated menu over ExSni", label: "[#{System.os_time(:millisecond)}]")
    %{adapter | menubar: menubar}
  end

  def popup_menu(adapter = %__MODULE__{}) do
    adapter
  end

  def recreate_menu(adapter = %__MODULE__{}, _dom) do
    adapter
  end

  def menubar(%__MODULE__{menubar: menubar}) do
    menubar
  end

  def get_icon(%__MODULE__{icon: icon}) do
    icon
  end

  def set_icon(adapter = %__MODULE__{}, nil) do
    {:ok, %{adapter | icon: nil}}
  end

  def set_icon(adapter = %__MODULE__{sni: sni}, icon) do
    with {:ok, _} <- set_sni_icon(sni, icon) do
      {:ok, %{adapter | icon: icon}}
    end
  end

  # Private functions

  defp set_sni_icon(sni, icon) do
    with {:ok, dbus_icon} <- generate_icon(icon),
         {:ok, icon} <- ExSni.update_icon(sni, dbus_icon) do
      {:ok, icon}
    end
  end

  defp create_menu_items(next_id, [], _adapter) do
    {[], next_id}
  end

  defp create_menu_items(next_id, {:menu, _, children}, adapter) do
    create_menu_items(next_id, children, adapter)
  end

  defp create_menu_items(next_id, [child | children], adapter) do
    {item, last_id} = create_menu_item(next_id, child, adapter)
    {items, last_id} = create_menu_items(last_id + 1, children, adapter)
    {[item | items], last_id}
  end

  defp create_menu_item(id, {:item, params, [label]}, adapter) do
    {create_standard_item(id, label, params, adapter), id}
  end

  defp create_menu_item(id, {:hr, _, _}, _opts) do
    # Separator is always visible. Has ID 1
    {Item.separator(), id}
  end

  defp create_menu_item(id, {:menu, params, children}, adapter) do
    {children, last_id} = create_menu_items(id + 1, children, adapter)
    item = create_submenu_item(id, params, children, adapter)
    # item = %{item | children: children}
    {item, last_id}
  end

  defp create_menu_item(id, {:menubar, _params, children}, adapter) do
    create_menu_items(id, children, adapter)
  end

  defp create_submenu_item(id, params, children, %{menu_pid: menu_pid}) do
    children
    |> Item.menu()
    |> Item.set_label(Map.get(params, :label, ""))
    |> Item.set_id(id)
    |> Item.set_uid(Map.get(params, :id, ""))
    |> Item.set_enabled(!param_disabled?(Map.get(params, :disabled, false)))
    |> Item.set_callbacks(build_callbacks(menu_pid, params))
  end

  defp create_standard_item(id, label, params, %{menu_pid: menu_pid}) do
    params
    |> Map.get(:type, nil)
    |> case do
      "checkbox" -> Item.checkbox(label)
      "radio" -> Item.radio(label)
      _ -> Item.standard(label)
    end
    |> Item.set_id(id)
    |> Item.set_uid(Map.get(params, :id, ""))
    |> Item.set_checked(param_checked?(Map.get(params, :checked, false)))
    |> Item.set_enabled(!param_disabled?(Map.get(params, :disabled, false)))
    |> Item.set_callbacks(build_callbacks(menu_pid, params))
  end

  defp build_callbacks(menu_pid, params) do
    if menu_pid != nil do
      Enum.reduce(params, [], fn param, acc ->
        case param do
          {:onclick, event} ->
            [
              {"clicked",
               fn _data, _timestamp ->
                 IO.inspect("On Menu Item click", label: "[#{System.os_time(:millisecond)}]")
                 spawn_link(Desktop.Menu, :trigger_event, [menu_pid, event])
               end}
              | acc
            ]

          _ ->
            acc
        end
      end)
    end
  end

  defp param_checked?(value) when is_binary(value) do
    case String.downcase(value) do
      "checked" -> true
      "true" -> true
      _ -> false
    end
  end

  defp param_checked?(_) do
    false
  end

  defp param_disabled?(value) when is_binary(value) do
    case String.downcase(value) do
      "disabled" -> true
      "true" -> true
      _ -> false
    end
  end

  defp param_disabled?(_) do
    false
  end

  defp generate_icon(nil) do
    {:ok, nil}
  end

  defp generate_icon(icon = %Icon{}) do
    {:ok, icon}
  end

  defp generate_icon(icon_name) when is_binary(icon_name) do
    icon = %{
      generate_sni_icon()
      | icon: %Info{
          name: icon_name
        }
    }

    {:ok, icon}
  end

  defp generate_icon(wx_icon) do
    :wxIcon = :wx.getObjectType(wx_icon)

    case Pixmap.from_wx_icon(wx_icon,
           env: Desktop.Env.wx_env(),
           rescale: true,
           width: 22,
           height: 22
         ) do
      {:ok, pixmap} ->
        icon = %{
          generate_sni_icon()
          | icon: %Info{
              data: {:pixmap, [pixmap]}
            }
        }

        {:ok, icon}

      error ->
        Logger.warn(error)
        error
    end
  end

  defp generate_sni_icon() do
    %Icon{
      category: :application_status,
      id: "1",
      title: "",
      menu: "/MenuBar",
      status: :active,
      tooltip: %Tooltip{
        name: "",
        title: "",
        description: ""
      }
    }
  end
end
