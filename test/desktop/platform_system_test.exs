defmodule Desktop.Platform.SystemTest do
  use ExUnit.Case, async: false

  alias Desktop.Platform.System, as: PlatformSystem

  defmodule StubBackend do
    # Minimal stand-in: Platform.System.os_description/0 uses with_wx_env/1, which may
    # call set_env/1 when Desktop.Env is up.
    def set_env(_env), do: :ok
    def os_description, do: Process.get(:stub_os_description)
  end

  setup do
    previous = Application.get_env(:desktop, :backend, :auto)
    Application.put_env(:desktop, :backend, StubBackend)

    on_exit(fn ->
      Application.put_env(:desktop, :backend, previous)
    end)

    :ok
  end

  test "os_description normalizes charlist and binary" do
    Process.put(:stub_os_description, ~c"Mock OS")
    assert PlatformSystem.os_description() == "Mock OS"

    Process.put(:stub_os_description, "Linux Desktop")
    assert PlatformSystem.os_description() == "Linux Desktop"
  end

  test "os_description trims blank values to nil" do
    Process.put(:stub_os_description, "   ")
    assert PlatformSystem.os_description() == nil

    Process.put(:stub_os_description, ~c"")
    assert PlatformSystem.os_description() == nil

    Process.put(:stub_os_description, nil)
    assert PlatformSystem.os_description() == nil
  end
end

defmodule Desktop.Platform.SystemCustomEventTest do
  use ExUnit.Case, async: false

  alias Desktop.Platform.System, as: PlatformSystem

  defmodule StubBackend do
    def set_env(_env), do: :ok

    def custom_event(event, args) do
      send(Process.get(:stub_test_pid), {:custom_event, event, args})
      :ok
    end
  end

  setup do
    previous = Application.get_env(:desktop, :backend, :auto)
    Application.put_env(:desktop, :backend, StubBackend)
    Process.put(:stub_test_pid, self())

    on_exit(fn ->
      Application.put_env(:desktop, :backend, previous)
    end)

    :ok
  end

  test "custom_event delegates to backend" do
    assert :ok = PlatformSystem.custom_event(:share, ["/tmp/a"])
    assert_receive {:custom_event, :share, ["/tmp/a"]}
  end
end
