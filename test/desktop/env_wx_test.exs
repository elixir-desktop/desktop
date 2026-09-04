defmodule Desktop.EnvWxTest do
  use Desktop.Test.WxCase

  test "T-ENV-04: Env stores wx and wx_env after init" do
    wx = Desktop.Env.wx()
    env = Desktop.Env.wx_env()

    assert wx != nil
    assert env != nil
  end

  test "T-ENV-04: wx_use_env succeeds" do
    assert :ok = Desktop.Env.wx_use_env()
  end
end
