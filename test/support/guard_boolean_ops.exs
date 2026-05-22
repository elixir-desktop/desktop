# Run via: mix test.guard
# Fails when known unsafe `and`/`or` patterns reappear in window.ex

window_file = Path.expand("../../lib/desktop/window.ex", __DIR__)
content = File.read!(window_file)

forbidden = [
  {~r/if\s+frame\s+and\s+/,
   "use `frame != nil && ...` — wx refs are not boolean (R4)"},
  {~r/if\s+menubar\s+and\s+/,
   "use `menubar && frame` — module atoms are not boolean (R3)"},
  {~r/if\s+frame\s+and\s+not\s+/, "use `frame != nil && !...` (R4)"}
]

violations =
  for {pattern, message} <- forbidden,
      line <- String.split(content, "\n"),
      Regex.match?(pattern, line) do
    {message, String.trim(line)}
  end

if violations != [] do
  IO.puts(:stderr, "guard_boolean_ops: FAILED\n")

  for {message, line} <- violations do
    IO.puts(:stderr, "  #{message}\n    #{line}\n")
  end

  System.halt(1)
else
  IO.puts("guard_boolean_ops: OK")
end
