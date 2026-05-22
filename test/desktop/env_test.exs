defmodule Desktop.EnvTest do
  use ExUnit.Case, async: false

  describe "browser backend" do
    test "T-ENV-03: wx_use_env with nil wx_env is no-op" do
      previous = Application.get_env(:desktop, :backend, :auto)
      Application.put_env(:desktop, :backend, :browser)

      try do
        {:ok, _} = Application.ensure_all_started(:desktop)
        assert :ok = Desktop.Env.wx_use_env()
      after
        Application.put_env(:desktop, :backend, previous)
      end
    end
  end

  describe "wx backend (T-ENV-04)" do
    use Desktop.Test.WxCase

    test "Env stores wx and wx_env after init" do
      wx = Desktop.Env.wx()
      env = Desktop.Env.wx_env()

      assert wx != nil
      assert env != nil
    end

    test "wx_use_env succeeds" do
      assert :ok = Desktop.Env.wx_use_env()
    end
  end
end
