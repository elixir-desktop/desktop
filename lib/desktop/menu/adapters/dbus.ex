defmodule Desktop.Menu.Adapter.DBus do
  require Logger

  alias ExSni.Icon
  alias ExSni.Icon.Info
  alias ExSni.Menu
  alias ExSni.Menu.Item
  alias Desktop.Menu.Pixmap

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
    {children, _} = create_menu_items(1, dom, adapter)

    root = %Item{id: 0, children: children}

    callbacks = [
      {:show,
       fn ->
         # ABOUT TO SHOW menubar
         # Return "needUpdate" = false
         false
       end}
    ]

    menubar = %Menu{root: root, callbacks: callbacks}

    ExSni.update_menu(sni, nil, menubar)

    %{adapter | menubar: menubar}
  end

  def popup_menu(adapter = %__MODULE__{}, _dom) do
    adapter
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

  defp create_menu_items(next_id, [], _opts) do
    {[], next_id}
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
    {%Item{id: id, type: "separator"}, id}
  end

  defp create_menu_item(id, {:menu, params, children}, adapter) do
    item = create_standard_item(id, Map.get(params, :label, ""), params, adapter)
    {children, last_id} = create_menu_items(id + 1, children, adapter)
    item = %{item | children: children}
    {item, last_id}
  end

  defp create_standard_item(id, label, params, %{menu_pid: menu_pid}) do
    toggle_type =
      case Map.get(params, :type, nil) do
        "checkbox" -> :checkmark
        "radio" -> :radio
        _ -> nil
      end

    toggle_state =
      case Map.get(params, :checked, nil) do
        "checked" -> :on
        _ -> :off
      end

    callbacks =
      if menu_pid != nil do
        Enum.reduce(params, [], fn param, acc ->
          case param do
            {:onclick, event} ->
              [
                {"clicked",
                 fn _, _ ->
                   spawn_link(Desktop.Menu, :trigger_event, [menu_pid, event])
                 end}
                | acc
              ]

            _ ->
              acc
          end
        end)
      end

    %Item{
      id: id,
      label: label,
      toggle_type: toggle_type,
      toggle_state: toggle_state,
      callbacks: callbacks
    }
  end

  defp generate_icon(nil) do
    {:ok, nil}
  end

  defp generate_icon(icon = %Icon{}) do
    {:ok, icon}
  end

  defp generate_icon(wx_icon = {:wx_ref, _, :wxIcon, _}) do
    with {:ok, pixmap} <- Pixmap.from_wxIcon(wx_icon, Desktop.Env.wx_env()) do
      icon = %Icon{
        category: :application_status,
        id: "1",
        title: "",
        menu: "/MenuBar",
        status: :active,
        icon: %Info{
          data: {:pixmap, pixmap}
        }
      }

      {:ok, icon}
    else
      error ->
        Logger.warn(error)
        error
    end
  end
end
