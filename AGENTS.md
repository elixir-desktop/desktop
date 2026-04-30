# AGENTS.md

## Cursor Cloud specific instructions

This is an **Elixir library** (not a standalone app). It provides native desktop windowing for Phoenix LiveView apps via wxWidgets.

### Toolchain

Erlang and Elixir versions are managed via `.tool-versions` (asdf format). The Cloud VM uses **mise** to resolve these. After the update script runs, `erl` and `elixir` are available on `PATH`.

### System dependencies

wxWidgets GUI support requires `libwxgtk-webview3.2-dev` and `xvfb` on headless Linux. These are pre-installed in the VM snapshot.

### Key commands

| Task | Command |
|---|---|
| Install deps | `mix deps.get` |
| Compile | `mix compile` |
| Lint (all) | `mix lint` (runs compile --warnings-as-errors, format --check-formatted, credo --ignore refactor, dialyzer) |
| Tests | `xvfb-run mix test` |
| Run code | `xvfb-run mix run -e '<elixir code>'` |
| IEx shell | `xvfb-run iex -S mix` |

### Known issues

- **Dialyzer warnings**: `mix dialyzer` emits pre-existing warnings in `lib/mix/tasks/desktop.install.ex` related to the optional `igniter` dependency. These also fail in CI on `main`.
- **Install test**: `test/mix/tasks/desktop.install_test.exs` fails because `Igniter.Test.phx_test_project/1` requires a working `mix phx.new` integration that does not fully resolve in all environments. This is pre-existing and CI does not run `mix test`.
- **GLib warning**: `xvfb-run` may emit a harmless `GLib-WARNING` about `g_spawn_sync()` / `ECHILD`; this is cosmetic and can be ignored.

### Running without a display

All commands that load the `:wx` application (compile, test, iex) need a display. Use `xvfb-run` prefix on headless servers. The `NO_WX` environment variable can skip wx initialization but will limit functionality.
