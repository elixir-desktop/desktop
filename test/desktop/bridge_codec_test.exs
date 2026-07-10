defmodule Desktop.BridgeCodecTest do
  use ExUnit.Case

  alias Desktop.Bridge.Codec

  test "round-trip encode/decode" do
    payload = [:wxFrame, :show, [[id: 1, type: :wxFrame], [show: true]]]
    json = Codec.encode!(payload)
    assert Codec.decode!(json) == payload
  end

  test "mock handles legacy rpc" do
    ref = 42
    json = Codec.encode!([:wxWebView, :isShown, [nil]])
    Desktop.Bridge.Mock.send(self(), <<ref::unsigned-size(64), json::binary>>)

    assert_receive {:tcp, Desktop.Bridge.Mock, <<^ref::unsigned-size(64), _reply::binary>>}
  end
end
