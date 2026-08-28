defmodule Desktop.WindowNotificationTest do
  use Desktop.Test.DesktopCase, async: true

  alias Desktop.Window

  test "edw_notification click invokes stored callback" do
    test = self()

    ui =
      minimal_window(
        notifications: %{
          "nid" => {"nid", fn action -> send(test, {:cb, action}) end}
        }
      )

    assert {:noreply, ^ui} = Window.handle_info({:edw_notification, "nid", :click}, ui)
    assert_receive {:cb, :click}, 500
  end

  test "edw_notification dismiss with nil callback is a no-op" do
    ui = minimal_window(notifications: %{"nid" => {"nid", nil}})

    assert {:noreply, ^ui} = Window.handle_info({:edw_notification, "nid", :dismiss}, ui)
  end

  test "edw_notification unknown id does not crash" do
    ui = minimal_window(notifications: %{})

    assert {:noreply, ^ui} = Window.handle_info({:edw_notification, "missing", :click}, ui)
  end

  test "show_notification stores under string key for native-style handles" do
    with_backend(:browser, fn ->
      # Browser new/2 returns nil; cast still stores under to_string(id).
      ui = minimal_window(notifications: %{})
      test = self()
      callback = fn action -> send(test, {:cb, action}) end

      assert {:noreply, ui2} =
               Window.handle_cast(
                 {:show_notification, "hello", :demo, :info, "Title", callback, -1},
                 ui
               )

      assert Map.has_key?(ui2.notifications, "demo")
      assert {_handle, ^callback} = ui2.notifications["demo"]

      assert {:noreply, _} = Window.handle_info({:edw_notification, "demo", :click}, ui2)
      assert_receive {:cb, :click}, 500
    end)
  end

  test "dismiss_notification pops string key" do
    with_backend(:browser, fn ->
      ui = minimal_window(notifications: %{"demo" => {nil, nil}})

      assert {:noreply, ui2} = Window.handle_cast({:dismiss_notification, :demo}, ui)
      assert ui2.notifications == %{}
    end)
  end

  test "show_notification converts native handle to string id for close correlation" do
    ui =
      minimal_window(
        notifications: %{
          "chat-1" => {{:notification, "Title", :info}, fn _ -> :ok end}
        }
      )

    with_backend(:browser, fn ->
      assert {:noreply, ui2} =
               Window.handle_cast(
                 {:show_notification, "body", "chat-1", :info, "Title", nil, -1},
                 ui
               )

      assert {"chat-1", nil} = ui2.notifications["chat-1"]
    end)
  end
end
