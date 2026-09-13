defmodule Desktop.MenuCrashTest do
  use ExUnit.Case, async: false

  alias Desktop.Menu
  alias Desktop.Menu.Adapter.Fake

  defmodule IconMenu do
    use Desktop.Menu, server: false

    def mount(menu), do: {:ok, menu}
    def handle_event(_event, menu), do: {:noreply, menu}
    def handle_info(_msg, menu), do: {:noreply, menu}

    def render(assigns) do
      ~H"""
      <menu>
        <item onclick="quit">Quit</item>
      </menu>
      """
    end
  end

  test "T-MENU-03: GenServer.stop destroys the taskbar icon" do
    Process.flag(:trap_exit, true)
    {:ok, pid} = start_menu()
    assert Process.alive?(pid)

    GenServer.stop(pid, :shutdown)
    assert_receive {:tray_destroyed, :tray}, 1000
    refute Process.alive?(pid)
  end

  test "T-MENU-04: parent crash destroys the taskbar icon" do
    test = self()

    parent =
      spawn(fn ->
        {:ok, pid} = start_menu(notify: test)
        send(test, {:menu, pid})

        receive do
          :crash -> exit(:crash)
        end
      end)

    assert_receive {:menu, menu}, 1000
    ref = Process.monitor(menu)

    ExUnit.CaptureLog.capture_log(fn ->
      send(parent, :crash)
      assert_receive {:tray_destroyed, :tray}, 1000
      assert_receive {:DOWN, ^ref, :process, ^menu, :crash}, 1000
    end)
  end

  test "T-MENU-05: non-parent EXIT does not destroy the taskbar icon" do
    Process.flag(:trap_exit, true)
    {:ok, pid} = start_menu()
    stranger = spawn(fn -> :ok end)

    send(pid, {:EXIT, stranger, :normal})
    send(pid, {:EXIT, stranger, :crash})

    refute_receive {:tray_destroyed, :tray}, 200
    assert Process.alive?(pid)

    GenServer.stop(pid, :shutdown)
    assert_receive {:tray_destroyed, :tray}, 1000
  end

  defp start_menu(opts \\ []) do
    notify = Keyword.get(opts, :notify, self())

    Menu.start_link(
      module: IconMenu,
      adapter: Fake,
      notify: notify,
      wx: {:taskbar, :icon},
      dom: []
    )
  end
end
