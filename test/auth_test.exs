defmodule Desktop.AuthTest do
  use ExUnit.Case, async: false

  setup do
    reset_auth_key!()
    :ok
  end

  test "login_key/0 is idempotent" do
    key1 = Desktop.Auth.login_key()
    key2 = Desktop.Auth.login_key()

    assert key1 == key2
    assert byte_size(key1) > 0
  end

  test "login_key/0 returns one key under concurrent first access" do
    parent = self()

    for _ <- 1..40 do
      spawn(fn ->
        send(parent, {:key, Desktop.Auth.login_key()})
      end)
    end

    keys =
      for _ <- 1..40 do
        receive do
          {:key, key} -> key
        after
          5_000 -> flunk("timeout waiting for auth key")
        end
      end

    assert length(Enum.uniq(keys)) == 1
    assert hd(keys) == Desktop.Auth.login_key()
  end

  test "set_key/1 is visible to login_key/0 and Auth plug" do
    raw = :crypto.strong_rand_bytes(32)
    encoded = Base.encode32(raw)
    login = Base.encode32(raw, padding: false)

    Desktop.Auth.set_key(encoded)
    assert Desktop.Auth.login_key() == login

    opts = Desktop.Auth.init([])

    conn =
      Plug.Test.conn(:get, "/?k=#{login}")
      |> Plug.Test.init_test_session(%{})
      |> Desktop.Auth.call(opts)

    refute conn.halted
  end

  test "Auth plug rejects missing key" do
    _ = Desktop.Auth.login_key()
    opts = Desktop.Auth.init([])

    conn =
      Plug.Test.conn(:get, "/")
      |> Plug.Test.init_test_session(%{})
      |> Desktop.Auth.call(opts)

    assert conn.halted
    assert conn.status == 401
    assert conn.resp_body == "Unauthorized"
  end

  defp reset_auth_key!() do
    :persistent_term.erase({Desktop.Auth, :key})

    case :ets.whereis(Desktop.Auth) do
      :undefined -> :ok
      _tid -> :ets.delete_all_objects(Desktop.Auth)
    end
  end
end
