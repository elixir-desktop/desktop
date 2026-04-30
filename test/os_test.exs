defmodule Desktop.OSTest do
  use ExUnit.Case

  describe "type/0 and ELIXIR_DESKTOP_OS" do
    test "macos override forces MacOS for tests and CI" do
      old = System.get_env("ELIXIR_DESKTOP_OS")

      try do
        System.put_env("ELIXIR_DESKTOP_OS", "macos")
        assert Desktop.OS.type() == MacOS
      after
        if old do
          System.put_env("ELIXIR_DESKTOP_OS", old)
        else
          System.delete_env("ELIXIR_DESKTOP_OS")
        end
      end
    end
  end
end
