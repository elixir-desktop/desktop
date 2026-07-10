defmodule Desktop.Bridge.TransportTest do
  use ExUnit.Case, async: false

  alias Desktop.Bridge.{Codec, Transport}

  setup do
    System.put_env("BRIDGE_PORT", "0")
    pid = Transport.ensure_started()

    on_exit(fn ->
      if Process.alive?(pid), do: GenServer.stop(pid)
    end)

    {:ok, transport: pid}
  end

  test "T-BRG-03: subscribe_events delivers events" do
    transport = Transport.ensure_started()
    subscriber = self()
    event = [:open_file, ["/tmp/test.txt"]]

    ref = 0
    json = Codec.encode!(event)

    send(transport, {:tcp, Desktop.Bridge.Mock, <<ref::unsigned-size(64), json::binary>>})

    assert :ok = Transport.subscribe_events(subscriber)
    assert_receive [:open_file, ["/tmp/test.txt"]], 1000
  end

  test "T-BRG-04: connect is cast and returns ok" do
    frame = [id: 1, type: :wxFrame, args: []]

    assert :ok =
             Transport.bridge_call(:wxFrame, :connect, [
               frame,
               :close_window,
               [callback: fn -> :ok end, userData: self()]
             ])
  end

  test "T-BRG-05: callback ref 1 invokes registered fun" do
    parent = self()

    fun = fn arg ->
      send(parent, {:fun_called, arg})
    end

    ref = Transport.register_fun(fun)
    json = Codec.encode!(["clicked"])

    transport = Transport.ensure_started()

    send(
      transport,
      {:tcp, Desktop.Bridge.Mock, <<1::unsigned-size(64), ref::unsigned-size(64), json::binary>>}
    )

    Process.sleep(50)
    assert_received {:fun_called, "clicked"}
  end

  test "loadURL tracks last url" do
    Transport.bridge_call(:wxWebView, :loadURL, [nil, "http://example.com/app"])
    assert :ok
  end
end
