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
| Tests (fast, no wx) | `mix test.fast` |
| Tests (wx only) | `xvfb-run -a mix test.wx` (or `DISPLAY=:99 mix test.wx` if xvfb is already running) |
| Tests (all) | `xvfb-run -a mix test` |
| Regression guard | `mix test.guard` |
| Run code | `xvfb-run mix run -e '<elixir code>'` |
| IEx shell | `xvfb-run iex -S mix` |

### Known issues

- **Dialyzer warnings**: `mix dialyzer` emits pre-existing warnings in `lib/mix/tasks/desktop.install.ex` related to the optional `igniter` dependency. These also fail in CI on `main`.
- **Install test**: `test/mix/tasks/desktop.install_test.exs` fails because `Igniter.Test.phx_test_project/1` requires a working `mix phx.new` integration that does not fully resolve in all environments. This is pre-existing and CI does not run `mix test`.
- **GLib warning**: `xvfb-run` may emit a harmless `GLib-WARNING` about `g_spawn_sync()` / `ECHILD`; this is cosmetic and can be ignored.

### Running without a display

All commands that load the `:wx` application (compile, test, iex) need a display. Use `xvfb-run` prefix on headless servers. The `NO_WX` environment variable selects the `Desktop.Backend.Browser` platform backend (OS browser fallback, no native window).

### Platform backends

| Backend | When |
|---|---|
| `Desktop.Backend.Wx` | Desktop host targets with OTP `:wx` (default) |
| `Desktop.Backend.Json` | `:mobile_target` compile config or `OS.mobile?/0` — JSON bridge via `BRIDGE_PORT` |
| `Desktop.Backend.Browser` | `NO_WX=1` or `:wx` unavailable |

Override with `config :desktop, :backend, :wx | :json | :browser | :auto` or a custom module.
Custom backends may set `config :desktop, :menu_adapter, Some.Menu.Adapter` (used by `desktop_webview`).

### Platform abstraction (do not regress)

Library code is layered:

1. **Public API** — `Desktop`, `Desktop.Window`, `Desktop.OS` (legacy helpers).
2. **Platform facades** — `Desktop.Platform.*` (route to the active backend).
3. **Backends** — `Desktop.Backend.Wx`, `.Json`, `.Browser` (implement behaviour).

**Rules for agents and contributors:**

| Do | Don't |
|---|---|
| Call `Desktop.Platform.System.locale/0`, `.open_external_url/1`, `Desktop.Platform.Window.*`, etc. from library code | Call `Desktop.Backend.*` or `:wx*` modules directly from `Window`, `Menu`, `OS`, etc. |
| Keep `Desktop.OS.launch_default_browser/1` as a thin spawn wrapper to `Platform.System.open_external_url/1` only in `os.ex` | Re-implement browser launch, locale, or wx setup in `OS` with `case OS.type()` / `wx_available?` branches |
| Let `Desktop.Platform.Helpers.with_wx_env/1` (used by `Platform.System` and `Platform.Window`) call `Desktop.Env.wx_use_env/0` before backend work | Call `Desktop.Env.wx_use_env/0` from app/library modules; never guard it with `if Platform.System.wx_available?()` |
| Rely on `wx_use_env/0` being a safe no-op when `Desktop.Env` is down or `wx_env` is nil | Assume wx is loaded on Json/mobile or skip Platform because “it's only wx” |
| On **Json/mobile**, use `Protocol.new/call/connect` with **bridge atoms** (e.g. `[:getSystemLanguage]`) | Put evaluated BEAM calls in RPC args (e.g. `Protocol.new(:wxLocale, [:wxLocale.getSystemLanguage()])` or `:wxLocale.getSystemLanguage/0` in `json.ex`) |
| Put wx-specific logic in the matching **backend** module | Put Android/iOS `:ok` stubs or `Null.wx_call` in `OS` or `Window` |

**Regression guards:** `mix test.guard` runs `guard_boolean_ops.exs` and `guard_platform_abstraction.exs` (forbidden patterns under `lib/`).

**Tests:** `mix test.fast` — no wx; `xvfb-run -a mix test.wx` — wx backend; `mix test.guard` — static rules.
