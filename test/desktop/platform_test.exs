defmodule Desktop.PlatformTest do
  use ExUnit.Case, async: true

  import Desktop.Test.DesktopCase

  alias Desktop.Platform
  alias Desktop.Backend.{Wx, Json, Browser}

  test "T-PLAT-01: backend returns Wx on host by default" do
    with_backend(:auto, fn ->
      assert Platform.backend() == Wx
    end)
  end

  test "T-PLAT-02: config override browser" do
    with_backend(:browser, fn ->
      assert Platform.backend() == Browser
    end)
  end

  test "T-PLAT-03: config override json" do
    with_backend(:json, fn ->
      assert Platform.backend() == Json
    end)
  end

  test "T-PLAT-04: NO_WX selects Browser" do
    with_env("NO_WX", "1", fn ->
      with_backend(:auto, fn ->
        assert Platform.backend() == Browser
      end)
    end)
  end

  test "T-PLAT-05: capabilities for Wx backend" do
    assert Wx.capabilities().window
    assert Wx.capabilities().content == :webview
  end

  test "T-PLAT-06: window_server on browser is Platform.Server" do
    with_backend(:browser, fn ->
      assert Platform.window_server() == Desktop.Platform.Server
    end)
  end

  test "menu adapter selection browser" do
    with_backend(:browser, fn ->
      assert Platform.Menu.adapter() == Desktop.Menu.Adapter.Browser
    end)
  end

  test "menu adapter selection wx" do
    with_backend(:wx, fn ->
      assert Platform.Menu.adapter() in [
               Desktop.Menu.Adapter.Wx,
               Desktop.Menu.Adapter.Browser
             ]
    end)
  end

  test "T-PLAT-07: mobile_target env selects Json without Mix" do
    previous = Application.get_env(:desktop, :backend, :auto)
    mobile? = Application.get_env(:desktop, :mobile_target)

    Application.put_env(:desktop, :backend, :auto)
    Application.put_env(:desktop, :mobile_target, true)
    System.put_env("ELIXIR_DESKTOP_OS", "android")

    try do
      assert Platform.backend() == Json
    after
      Application.put_env(:desktop, :backend, previous)
      Application.put_env(:desktop, :mobile_target, mobile?)
      System.delete_env("ELIXIR_DESKTOP_OS")
    end
  end
end
