defmodule Desktop.PlatformTest do
  use ExUnit.Case

  alias Desktop.Platform
  alias Desktop.Backend.{Wx, Json, Browser}

  test "backend returns Wx on host by default" do
    assert Platform.backend() == Wx
  end

  test "capabilities for Wx backend" do
    assert Wx.capabilities().window
    assert Wx.capabilities().content == :webview
  end

  test "capabilities for Browser backend" do
    assert Browser.capabilities().content == :os_browser
    assert Browser.capabilities().menu == :none
  end

  test "capabilities for Json backend" do
    assert Json.capabilities().content == :native
    assert Json.capabilities().menu == :native
  end

  test "config override backend" do
    Application.put_env(:desktop, :backend, :browser)

    try do
      assert Platform.backend() == Browser
    after
      Application.delete_env(:desktop, :backend)
    end
  end

  test "menu adapter selection" do
    assert Platform.Menu.adapter() in [
             Desktop.Menu.Adapter.Wx,
             Desktop.Menu.Adapter.Browser
           ]
  end
end
