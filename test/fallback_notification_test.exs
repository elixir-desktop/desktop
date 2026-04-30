defmodule Desktop.FallbackNotificationTest do
  use ExUnit.Case

  describe "notification_new/3" do
    test "parent option does not crash when wx is disabled (NO_WX)" do
      old_no_wx = System.get_env("NO_WX")
      old_os = System.get_env("ELIXIR_DESKTOP_OS")

      on_exit(fn ->
        restore_env("NO_WX", old_no_wx)
        restore_env("ELIXIR_DESKTOP_OS", old_os)
      end)

      System.put_env("NO_WX", "1")
      System.put_env("ELIXIR_DESKTOP_OS", "macos")

      assert Desktop.OS.macos?()
      # Parent is ignored when notification object cannot be created; must not raise.
      assert Desktop.Fallback.notification_new("Title", :info, :not_a_wx_window) == nil
    end
  end

  defp restore_env(key, nil), do: System.delete_env(key)
  defp restore_env(key, value), do: System.put_env(key, value)
end
