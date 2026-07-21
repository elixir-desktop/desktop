defmodule Desktop.Backend.JsonTest do
  use ExUnit.Case, async: false

  alias Desktop.Backend.Json
  alias Desktop.Bridge.Transport

  setup do
    previous = Application.get_env(:desktop, :backend, :auto)
    Application.put_env(:desktop, :backend, :json)
    System.put_env("BRIDGE_PORT", "0")
    Transport.ensure_started()

    on_exit(fn ->
      Application.put_env(:desktop, :backend, previous)
    end)

    :ok
  end

  test "capabilities" do
    caps = Json.capabilities()
    assert caps.content == :native
    assert caps.menu == :native
  end

  test "T-JSN: locale via bridge RPC only" do
    assert Json.locale() == "en"
  end

  test "T-JSN: os_description via bridge RPC" do
    assert Json.os_description() == ~c"Mock OS"
    assert Desktop.Platform.System.os_description() == "Mock OS"
  end

  test "T-JSN: new frame handle shape" do
    wx = Transport.ensure_started()

    {:ok, frame, webview} =
      Json.open(wx: wx, title: "test", size: {400, 300}, icon: nil)

    assert frame[:type] == :wxFrame
    assert is_integer(frame[:id])
    assert webview[:type] == :wxWebView
  end

  test "T-JSN: connect returns ok" do
    wx = Transport.ensure_started()
    {:ok, frame, _} = Json.open(wx: wx, title: "t", size: {200, 200}, icon: nil)

    assert :ok = Json.connect(frame, :close_window, fn _, _ -> :ok end)
  end

  test "T-JSN: loadURL via content_show" do
    wx = Transport.ensure_started()
    {:ok, frame, webview} = Json.open(wx: wx, title: "t", size: {200, 200}, icon: nil)

    assert :ok = Json.content_show(webview, frame, "http://localhost:4000", false)
  end
end
