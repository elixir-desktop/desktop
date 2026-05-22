defmodule Desktop.MenuAdapterTest do
  use ExUnit.Case, async: true

  alias Desktop.Menu.Adapter.{Browser, Json}
  alias Desktop.Bridge.Transport

  test "T-MENU-01: Browser adapter is no-op" do
    adapter = Browser.new(menu_pid: self())
    dom = {:menubar, %{}, []}

    adapter = Browser.create(adapter, dom)
    adapter = Browser.update_dom(adapter, dom)
    assert Browser.menubar(adapter) == nil
    assert {:ok, ^adapter} = Browser.set_icon(adapter, nil)
  end

  describe "Json adapter (T-MENU-02)" do
    setup do
      System.put_env("BRIDGE_PORT", "0")
      Transport.ensure_started()
      :ok
    end

    test "minimal menubar dom" do
      menubar = Desktop.Bridge.Protocol.new(:wxMenuBar, [])

      adapter =
        Json.new(
          menu_pid: self(),
          env: :ok,
          wx: menubar
        )

      dom = {:menubar, %{}, [{:menu, %{label: "File"}, [{:item, %{onclick: "quit"}, ["Quit"]}]}]}

      adapter = Json.create(adapter, dom)
      assert adapter.menubar != nil
    end
  end
end
