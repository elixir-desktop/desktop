defmodule Desktop.WindowLifecycleTest do
  use Desktop.Test.DesktopCase, async: true

  alias Desktop.Window

  test "T-WIN-01: prepare_url appends auth key" do
    expected = "/some/?k=" <> Desktop.Auth.login_key()
    assert Window.prepare_url("/some/") == expected
  end

  test "T-WIN-02: hide and set_title with nil frame" do
    ui = minimal_window(frame: nil)

    with_backend(:browser, fn ->
      assert {:noreply, ^ui} = Window.handle_cast(:hide, ui)
      assert {:noreply, _} = Window.handle_cast({:set_title, "New"}, ui)
    end)
  end

  test "T-WIN-03: close_window branches" do
    with_backend(:browser, fn ->
      ui_hide = minimal_window(frame: fake_frame(), on_close: :hide)
      assert {:noreply, ^ui_hide} = Window.handle_cast(:close_window, ui_hide)

      ui_tray =
        minimal_window(frame: fake_frame(), taskbar: self(), on_close: :quit)

      assert {:noreply, ^ui_tray} = Window.handle_cast(:close_window, ui_tray)
    end)
  end

  describe "T-WIN-04 wx integration" do
    use Desktop.Test.WxCase

    @moduletag timeout: 10_000

    test "start_link and close_window cast keeps process alive" do
      name = :"window_test_#{System.unique_integer([:positive])}"

      {:ok, pid} =
        Window.start_link(
          app: :desktop,
          id: name,
          title: "Lifecycle",
          size: {400, 300},
          hidden: true
        )

      assert Process.alive?(pid)
      GenServer.cast(pid, :close_window)
      Process.sleep(200)
      assert Process.alive?(pid)
      GenServer.stop(pid)
    end
  end
end
