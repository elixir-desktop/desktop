defmodule Desktop.Menu.Adapter.DBus do
  alias ExSni.Icon
  alias ExSni.Icon.{Info, Tooltip}
  alias ExSni.Menu
  alias ExSni.Menu.Item

  defstruct [:mod, :env, :sni, :icon, :menubar]

  def new(env, module) do
    %__MODULE__{
      mod: module,
      env: env,
      sni: nil,
      icon: dummy_icon(),
      menubar: nil
    }
  end

  def create(%__MODULE__{env: env, menubar: menubar, icon: icon} = menu, dom, opts) do
    sni = Keyword.get(opts, :sni)
    ExSni.set_icon(sni, icon)
    ExSni.set_menu(sni, menubar)
    ExSni.register_icon(sni)
    %{menu | sni: sni}
  end

  def update_dom(%__MODULE__{mod: module, menubar: menubar, sni: sni} = menu, dom) do
    {children, _} = create_menu_items(1, dom, module: module)
    root = %Item{id: 0, children: children}

    callbacks = [
      {:show,
       fn ->
         IO.inspect(false, label: "ABOUT TO SHOW menubar")
       end}
    ]

    menubar = %Menu{root: root, callbacks: callbacks}

    ExSni.update_menu(sni, nil, menubar)

    %{menu | menubar: menubar}
  end

  def popup_menu(%__MODULE__{} = menu, _dom) do
    menu
  end

  defp create_menu_items(next_id, [], _opts) do
    {[], next_id}
  end

  defp create_menu_items(next_id, [child | children], opts) do
    {item, last_id} = create_menu_item(next_id, child, opts)
    {items, last_id} = create_menu_items(last_id + 1, children, opts)
    {[item | items], last_id}
  end

  defp create_menu_item(id, {:item, params, [label]}, opts) do
    {create_standard_item(id, label, params, opts), id}
  end

  defp create_menu_item(id, {:hr, _, _}, _opts) do
    {%Item{id: id, type: "separator"}, id}
  end

  defp create_menu_item(id, {:menu, params, children}, opts) do
    item = create_standard_item(id, Map.get(params, :label, ""), params, opts)
    {children, last_id} = create_menu_items(id + 1, children, opts)
    item = %{item | children: children}
    {item, last_id}
  end

  defp create_standard_item(id, label, params, opts \\ []) do
    module = Keyword.get(opts, :module, nil)
    menu = Keyword.get(opts, :menu, nil)

    callbacks =
      if is_atom(module) do
        Enum.reduce(params, [], fn param, acc ->
          case param do
            {:onclick, event} ->
              [
                {"clicked",
                 fn _, _ ->
                   module.handle_event(event, menu)
                 end}
                | acc
              ]

            _ ->
              acc
          end
        end)
      end

    %Item{id: id, label: label, callbacks: callbacks}
  end

  defp dummy_icon() do
    %Icon{
      category: :application_status,
      id: "1",
      title: "Test_Icon",
      menu: "/MenuBar",
      status: :active,
      icon: %Info{
        name: "applications-development"
      },
      tooltip: %Tooltip{
        name: "applications-development",
        title: "test-tooltip",
        description: "Some tooltip description here"
      }
    }
  end
end
