defmodule Desktop.LanguageCodeTest do
  use ExUnit.Case, async: false

  alias Desktop

  test "T-ENV-01: language_code with Browser backend does not require wx" do
    previous = Application.get_env(:desktop, :backend, :auto)
    Application.put_env(:desktop, :backend, :browser)

    try do
      result = Desktop.language_code()
      assert result == nil or is_binary(result)
    after
      Application.put_env(:desktop, :backend, previous)
    end
  end

  describe "with wx (T-ENV-02)" do
    use Desktop.Test.WxCase

    test "language_code after Env start does not raise unknown_env" do
      result = Desktop.language_code()
      assert result == nil or is_binary(result)
    end
  end
end
