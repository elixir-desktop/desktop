defmodule Desktop.Regression.BeamWxCallsTest do
  @moduledoc """
  Guards against calling OTP :wx modules on the BEAM when using Json/mobile backends.
  """
  use ExUnit.Case, async: false

  alias Desktop.Backend.Json
  alias Desktop.Bridge.Transport
  alias Desktop.Platform.System, as: PlatformSystem

  setup do
    previous_backend = Application.get_env(:desktop, :backend, :auto)
    Application.put_env(:desktop, :backend, :json)
    System.put_env("BRIDGE_PORT", "0")
    System.put_env("ELIXIR_DESKTOP_OS", "android")
    Transport.ensure_started()

    on_exit(fn ->
      Application.put_env(:desktop, :backend, previous_backend)
      System.delete_env("ELIXIR_DESKTOP_OS")
    end)

    :ok
  end

  test "Json backend reports wx unavailable on BEAM" do
    refute Json.wx_available?()
  end

  test "locale does not invoke :wxLocale on BEAM" do
    assert Json.locale() == "en"
  end

  test "open_external_url on Json uses bridge RPC" do
    assert :ok = Json.open_external_url("https://example.com")
    assert :ok = PlatformSystem.open_external_url("https://example.com")
  end

  test "activate_event_active? on Json backend returns true without wx" do
    assert Json.activate_event_active?(%{})
    assert PlatformSystem.activate_event_active?(%{})
  end

  test "launch_default_browser delegates to Platform on Android" do
    assert is_pid(Desktop.OS.launch_default_browser("https://example.com"))
    Process.sleep(50)
  end
end
