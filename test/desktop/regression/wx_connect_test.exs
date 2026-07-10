defmodule Desktop.Regression.WxConnectTest do
  use ExUnit.Case, async: false

  alias Desktop.Bridge.Codec

  describe "Json connect RPC (T-CONN-02)" do
    setup do
      Desktop.Bridge.Transport.ensure_started()
      :ok
    end

    test "connect encodes callback and userData in args list" do
      frame = [id: 1, type: :wxFrame, args: []]
      pid = self()
      fun = fn _, _ -> :ok end

      json =
        Codec.encode!([
          :wxFrame,
          :connect,
          [frame, :close_window, [callback: fun, userData: pid]]
        ])

      decoded = Codec.decode!(json)
      assert [:wxFrame, :connect, [^frame, :close_window, opts]] = decoded
      assert Keyword.has_key?(opts, :callback)
      assert Keyword.fetch!(opts, :userData) == pid
    end
  end

  describe "Wx connect (T-CONN-01)" do
    use Desktop.Test.WxCase

    test "Backend.Wx.connect does not raise on close_window" do
      previous = Application.get_env(:desktop, :backend, :auto)
      Application.put_env(:desktop, :backend, :wx)

      try do
        wx = Desktop.Env.wx()

        {:ok, frame, _webview} =
          Desktop.Platform.Window.open(
            wx: wx,
            title: ~c"connect test",
            size: {200, 200},
            icon: nil
          )

        assert :ok =
                 Desktop.Backend.Wx.connect(frame, :close_window, fn _, _ ->
                   :ok
                 end)

        Desktop.Platform.Window.destroy(frame)
      after
        Application.put_env(:desktop, :backend, previous)
      end
    end
  end
end
