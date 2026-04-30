defmodule Desktop.ToolVersionsTest do
  use ExUnit.Case, async: true

  test "parse ignores comments and blank lines" do
    assert Desktop.ToolVersions.parse("""
           # comment
           elixir 1.19.1

           erlang 26.2.5.5 system
           """) == %{"elixir" => "1.19.1", "erlang" => "26.2.5.5"}
  end

  test "parse normalizes tool names to lowercase" do
    assert Desktop.ToolVersions.parse("Elixir 1.2.3") == %{"elixir" => "1.2.3"}
  end
end
