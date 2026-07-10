defmodule Desktop.Guard.BooleanOpsTest do
  use ExUnit.Case, async: true

  @window_file Path.expand("../../lib/desktop/window.ex", __DIR__)

  @forbidden [
    {~r/if\s+frame\s+and\s+/, "use frame != nil && ... (R4)"},
    {~r/if\s+menubar\s+and\s+/, "use menubar && frame (R3)"},
    {~r/if\s+frame\s+and\s+not\s+/, "use frame != nil && !... (R4)"}
  ]

  test "L0-GUARD: window.ex has no known unsafe and/or patterns" do
    content = File.read!(@window_file)

    violations =
      for {pattern, message} <- @forbidden,
          line <- String.split(content, "\n"),
          Regex.match?(pattern, line) do
        {message, String.trim(line)}
      end

    assert violations == [],
           "unsafe boolean ops:\n#{inspect(violations, pretty: true)}"
  end
end
