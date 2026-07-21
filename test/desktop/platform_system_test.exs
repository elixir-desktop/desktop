defmodule Desktop.Platform.SystemTest do
  use ExUnit.Case, async: false

  alias Desktop.Platform.System, as: PlatformSystem

  defmodule StubBackend do
    def os_description, do: Process.get(:stub_os_description)
  end

  setup do
    previous = Application.get_env(:desktop, :backend, :auto)
    Application.put_env(:desktop, :backend, StubBackend)

    on_exit(fn ->
      Application.put_env(:desktop, :backend, previous)
      Process.delete(:stub_os_description)
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
