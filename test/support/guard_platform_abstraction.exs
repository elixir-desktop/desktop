# Run via: mix test.guard
# Fails when platform abstraction rules are violated under lib/

lib_root = Path.expand("../../lib", __DIR__)

wx_use_env_allowed =
  ~w(
    desktop/env.ex
    desktop/platform/helpers.ex
    desktop/platform/system.ex
    desktop/platform/window.ex
  )

forbidden_patterns = [
  {~r/if\s+Desktop\.Platform\.System\.wx_available\?\s*\(\)/,
   "do not gate wx_use_env on wx_available? — call Platform APIs or wx_use_env (safe no-op)"},
  {~r/ensure_wx_env/,
   "use Desktop.Platform.Helpers.with_wx_env/1 via Platform facades, not ensure_wx_env"},
  {~r/:wxLocale\.getSystemLanguage\s*\(/,
   "Json/mobile: pass :getSystemLanguage as a bridge atom to Protocol, never :wxLocale.getSystemLanguage/0"},
  {~r/:wx_misc\.getOsDescription\s*\(/,
   "use Desktop.Platform.System.os_description/0 instead of :wx_misc.getOsDescription/0"}
]

violations =
  Enum.flat_map(Path.wildcard(Path.join(lib_root, "**/*.ex")), fn path ->
    rel = Path.relative_to(path, lib_root)
    content = File.read!(path)

    pattern_hits =
      for {pattern, message} <- forbidden_patterns,
          line <- String.split(content, "\n"),
          Regex.match?(pattern, line) do
        {rel, message, String.trim(line)}
      end

    extra =
      cond do
        String.contains?(content, "Desktop.Env.wx_use_env") and rel not in wx_use_env_allowed ->
          [{rel, "call Desktop.Platform.* instead of Desktop.Env.wx_use_env/0 in app code", ""}]

        rel != "desktop/os.ex" and Regex.match?(~r/Desktop\.OS\.launch_default_browser/, content) ->
          [
            {rel,
             "use Desktop.Platform.System.open_external_url/1 (OS.launch_default_browser only in os.ex)",
             ""}
          ]

        true ->
          []
      end

    pattern_hits ++ extra
  end)

if violations != [] do
  IO.puts(:stderr, "guard_platform_abstraction: FAILED\n")

  for {file, message, line} <- violations do
    IO.puts(:stderr, "  #{file}: #{message}")

    if line != "" do
      IO.puts(:stderr, "    #{line}")
    end
  end

  System.halt(1)
else
  IO.puts("guard_platform_abstraction: OK")
end
