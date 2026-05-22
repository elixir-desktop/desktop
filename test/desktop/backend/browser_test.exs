defmodule Desktop.Backend.BrowserTest do
  use ExUnit.Case, async: true

  alias Desktop.Backend.Browser

  test "capabilities" do
    caps = Browser.capabilities()
    assert caps.window == false
    assert caps.content == :os_browser
    assert caps.menu == :none
  end

  test "T-BRW: all Window callbacks return without raise" do
    assert {:ok, nil, nil} = Browser.open(wx: nil, title: "t", size: {100, 100})
    assert :ok = Browser.hide(nil)
    assert :ok = Browser.show(nil, [])
    assert :ok = Browser.set_title(nil, "x")
    assert :ok = Browser.connect(nil, :close_window, fn -> :ok end)
    assert Browser.is_shown?(nil) == false
    assert Browser.is_active?(nil) == false
  end

  test "T-BRW: Content callbacks" do
    assert :ok = Browser.content_show(nil, nil, "http://example.com", false)
    assert Browser.current_url(nil, "http://last") == "http://last"
    assert Browser.rebuild(nil, nil) == nil
  end

  test "T-BRW: Notification callbacks" do
    assert :ok = Browser.notification_show(nil, "msg", 0, "title")
    assert :ok = Browser.close(nil)
  end

  test "T-BRW: System callbacks" do
    assert {nil, nil} = Browser.init_env()
    assert :ok = Browser.subscribe_events()
    assert :ok = Browser.set_env(nil)
    assert Browser.get_env() == nil
    assert Browser.locale() == nil
    refute Browser.wx_available?()
  end
end
