defmodule Desktop.Menu.Adapter.DBus do
  @moduledoc false

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
    children = create_menu_items(dom, adapter)

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

    ExSni.update_menu(sni, nil, menubar)

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
    ExSni.update_icon(sni, generate_icon(icon))
  end

  defp create_menu_items([], _adapter) do
    []
  end

  defp create_menu_items({:menu, _, children}, adapter) do
    create_menu_items(children, adapter)
  end

  defp create_menu_items([child | children], adapter) do
    item = create_menu_item(child, adapter)
    items = create_menu_items(children, adapter)
    [item | items]
  end

  defp create_menu_item({:item, params, [label]}, adapter) do
    create_standard_item(label, params, adapter)
  end

  defp create_menu_item({:hr, _, _}, _opts) do
    Item.separator()
  end

  defp create_menu_item({:menu, params, children}, adapter) do
    children = create_menu_items(children, adapter)
    create_submenu_item(params, children, adapter)
  end

  defp create_menu_item({:menubar, _params, children}, adapter) do
    create_menu_items(children, adapter)
  end

  defp create_submenu_item(params, children, %{menu_pid: menu_pid}) do
    children
    |> Item.menu()
    |> Item.set_label(Map.get(params, :label, ""))
    |> Item.set_uid(Map.get(params, :id, ""))
    |> Item.set_enabled(!param_disabled?(Map.get(params, :disabled, false)))
    |> Item.set_callbacks(build_callbacks(menu_pid, params))
  end

  defp create_standard_item(label, params, %{menu_pid: menu_pid}) do
    params
    |> Map.get(:type, nil)
    |> case do
      "checkbox" -> Item.checkbox(label)
      "radio" -> Item.radio(label)
      _ -> Item.standard(label)
    end
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
              {
                "clicked",
                fn _data, _timestamp ->
                  Desktop.Menu.trigger_event(menu_pid, event)
                end,
                # Store on<event> attr pair after the callback for use in diffing
                {"onclick", event}
              }
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
    nil
  end

  defp generate_icon(icon = %Icon{}) do
    icon
  end

  defp generate_icon(icon_name) when is_binary(icon_name) do
    %{
      generate_sni_icon()
      | icon: %Info{
          name: icon_name
        }
    }
  end

  defp generate_icon(wx_icon) do
    :wxIcon = :wx.getObjectType(wx_icon)

    pixmap =
      Pixmap.from_wx_icon(wx_icon,
        env: Desktop.Env.wx_env(),
        rescale: true,
        width: 22,
        height: 22
      )

    %{
      generate_sni_icon()
      | icon: %Info{
          data: {:pixmap, [pixmap]}
        }
    }
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
