defmodule Desktop.ToolVersionsTest do
  use ExUnit.Case, async: true

  alias Desktop.ToolVersions

  test "parse ignores comments and blank lines" do
    content = """
    # comment
    erlang 26.2.5.5 system

    elixir 1.19.1-otp-26
    """

    assert ToolVersions.parse(content) == %{
             erlang: "26.2.5.5 system",
             elixir: "1.19.1-otp-26"
           }
  end

  test "parse handles extra whitespace on values" do
    content = "elixir  1.12.3 \n"

    assert ToolVersions.parse(content) == %{elixir: "1.12.3"}
  end

  test "parse empty file" do
    assert ToolVersions.parse("") == %{}
  end
end
