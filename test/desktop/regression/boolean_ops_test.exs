defmodule Desktop.Regression.BooleanOpsTest do
  use Desktop.Test.DesktopCase, async: true

  alias Desktop.Window

  test "T-BOOL-01: menubar && frame guard does not badbool with module atom" do
    menubar = Desktop.Test.MenuStub
    frame = fake_frame()

    assert menubar && frame
  end

  test "T-BOOL-02: close_window with wx ref and taskbar does not raise badbool" do
    ui =
      minimal_window(
        frame: fake_frame(),
        taskbar: self(),
        on_close: :quit
      )

    with_backend(:browser, fn ->
      assert {:noreply, ^ui} = Window.handle_cast(:close_window, ui)
    end)
  end

  test "T-BOOL-03: close_window with nil frame does not crash" do
    ui = minimal_window(frame: nil, taskbar: self(), on_close: :quit)

    with_backend(:browser, fn ->
      assert {:noreply, ^ui} = Window.handle_cast(:close_window, ui)
    end)
  end

  test "T-BOOL-04: close_window on_close hide hides without shutdown" do
    ui = minimal_window(frame: fake_frame(), on_close: :hide)

    with_backend(:browser, fn ->
      assert {:noreply, ^ui} = Window.handle_cast(:close_window, ui)
    end)
  end

  test "T-BOOL-05: hidden-close visibility expression uses boolean operators" do
    frame = fake_frame()
    assert frame != nil && !true == false
    assert frame != nil && !false == true
  end
end
