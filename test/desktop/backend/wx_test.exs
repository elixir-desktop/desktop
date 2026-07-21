defmodule Desktop.Backend.WxTest do
  use Desktop.Test.WxCase

  alias Desktop.Backend.Wx

  test "capabilities" do
    caps = Wx.capabilities()
    assert caps.window
    assert caps.content == :webview
  end

  @tag timeout: 10_000
  test "T-WX: open and destroy frame" do
    wx = Desktop.Env.wx()

    {:ok, frame, webview} =
      Wx.open(
        wx: wx,
        title: ~c"wx test",
        size: {320, 240},
        icon: nil
      )

    assert frame != nil
    assert is_reference(webview) or is_tuple(webview) or webview == nil
    assert :ok = Wx.destroy_frame(frame)
  end

  @tag timeout: 10_000
  test "T-WX: connect close_window" do
    wx = Desktop.Env.wx()
    {:ok, frame, _} = Wx.open(wx: wx, title: ~c"t", size: {200, 200}, icon: nil)

    assert :ok = Wx.connect(frame, :close_window, fn _, _ -> :ok end)
    Wx.destroy_frame(frame)
  end

  test "T-WX: locale via Platform.System" do
    result = Wx.locale()
    assert result == nil or is_binary(result)
  end

  test "T-WX: os_description via Platform.System" do
    result = Desktop.Platform.System.os_description()
    assert result == nil or is_binary(result)
  end
end
