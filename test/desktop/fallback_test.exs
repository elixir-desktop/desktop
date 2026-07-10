defmodule Desktop.FallbackTest do
  use Desktop.Test.DesktopCase, async: true

  alias Desktop.Fallback

  test "T-FALL-01: webview_load with nil webview does not raise" do
    window = minimal_window(frame: nil, webview: nil, last_url: nil)

    with_backend(:browser, fn ->
      assert Fallback.webview_load(window, "http://example.com")
    end)
  end

  test "wx_new returns pid or nil" do
    with_backend(:browser, fn ->
      assert Fallback.wx_new([]) == nil
    end)
  end

  test "notification_show with nil notification logs" do
    assert :ok = Fallback.notification_show(nil, "hello", -1, "title")
  end
end
