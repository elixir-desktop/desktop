# Desktop Platform — Test Design Plan

This document defines test coverage for the Platform/Backend refactor, based on production regressions found in `desktop-example-app` and the gaps in the current suite (23 tests, mostly parser/toolchain/codec).

## Regressions that must never recur

| ID | Symptom | Root cause | Regression test ID |
|----|---------|------------|-------------------|
| R1 | `{:wx, :unknown_env}` on app start | `language_code/0` → `Wx.locale/0` before `:wx.set_env/1` | T-ENV-01, T-ENV-02 |
| R2 | `function_clause` in `wxEvtHandler.parse_opts` | `connect/3` passed bare `callback:` instead of `[callback:, userData:]` | T-CONN-01, T-CONN-02 |
| R3 | `{:badbool, :and, TodoApp.MenuBar}` | `if menubar and frame` — non-boolean `and` operand | T-BOOL-01, L0-GUARD |
| R4 | `{:badbool, :and, wx_ref}` on window close | `if frame and not is_shown?` | T-BOOL-02, T-WIN-03 |

## Risk patterns to audit (grep / Credo)

- `if <non_boolean> and|or <expr>` where LHS is not a comparison (module atom, wx ref, pid)
- `:wx*` calls without prior `Desktop.Env.wx_use_env/0` or `Backend.Wx.ensure_wx_env/0`
- `Platform.Window.connect/3` not passing `userData: self()`
- `handle_cast` branches assuming `frame` is boolean

Safe patterns (do not “fix”):

- `if caps.window and not OS.mobile?()` — both booleans
- `if title != old and frame != nil` — both comparisons
- `if frame, do:` — `if` accepts truthy values (not `and`/`or`)

---

## Test architecture (4 layers)

```
L0 Static guards     → mix test.guard / Credo (no display)
L1 Unit (no wx)      → Browser backend, router, pure Window helpers
L2 Unit (mocked)     → Bridge.Mock, handle_cast with fake wx_ref
L3 Integration (:wx)  → xvfb-run mix test.wx
L4 App smoke         → desktop-example-app (optional, manual/CI nightly)
```

| Layer | Command | Catches |
|-------|---------|---------|
| L0 | `mix test.guard` | Future `and`/`or` misuse in Window |
| L1 | `mix test.fast` | Router, Browser contracts, cast logic |
| L2 | `mix test.fast` | Bridge wire format, close_window without GTK |
| L3 | `xvfb-run mix test.wx` | Real wx env, connect, locale, window lifecycle |
| L4 | script / manual | Full TodoApp supervision order |

---

## Test infrastructure

### `test/support/desktop_case.ex`

- `fake_frame/0` → `{:wx_ref, 1, :wxFrame, []}`
- `minimal_window/1` → `%Desktop.Window{...}` for cast tests
- `with_backend/2` — temporary `Application.put_env(:desktop, :backend, ...)`

### `test/support/wx_case.ex`

- `@moduletag :wx`
- Setup: ensure `Desktop.Env` started, `wx_use_env` available
- Document: requires `xvfb-run` on headless Linux

### Mix aliases (`mix.exs`)

```elixir
"test.fast": ["test --exclude wx"],
"test.wx": ["test --only wx"],
"test.guard": ["run test/support/guard_boolean_ops.exs"]
```

---

## Coverage matrix

### Platform router — `test/desktop/platform_test.exs` (extend)

| ID | Test |
|----|------|
| T-PLAT-01 | `backend/0` default Wx on host |
| T-PLAT-02 | `config :desktop, :backend` override `:browser`, `:json` |
| T-PLAT-03 | `NO_WX=1` → Browser |
| T-PLAT-04 | `Menu.adapter(sni: pid)` → DBus on Wx |
| T-PLAT-05 | `capabilities/0` consistent with active backend |
| T-PLAT-06 | `window_server/0` Wx vs Platform.Server |

### Regression — `test/desktop/regression/boolean_ops_test.exs` (new)

| ID | Test |
|----|------|
| T-BOOL-01 | Init guard: `menubar && frame` path does not raise with module + fake_frame |
| T-BOOL-02 | `handle_cast(:close_window)` with fake_frame + taskbar pid — no badbool (R4) |
| T-BOOL-03 | `handle_cast(:close_window)` with `frame: nil` — no crash |
| T-BOOL-04 | `on_close: :hide` branch calls hide, does not shutdown |

### Regression — `test/desktop/regression/wx_connect_test.exs` (new)

| ID | Test |
|----|------|
| T-CONN-01 | `@tag :wx` — `Backend.Wx.connect(frame, :close_window, fn -> :ok end)` does not raise |
| T-CONN-02 | Json + `BRIDGE_PORT=0` — connect RPC JSON third arg is keyword list with `:callback`, `:userData` |

### Locale / Env — `test/desktop/language_code_test.exs`, `env_test.exs` (new)

| ID | Test |
|----|------|
| T-ENV-01 | With Browser backend, `language_code/0` returns without wx (nil or string) |
| T-ENV-02 | `@tag :wx` — after `Env.start_link`, `language_code/0` no `:unknown_env` |
| T-ENV-03 | `wx_use_env/0` when `wx_env` is nil — no raise |
| T-ENV-04 | `@tag :wx` — `init` stores wx + wx_env |

### Window lifecycle — `test/desktop/window_lifecycle_test.exs` (new)

| ID | Test |
|----|------|
| T-WIN-01 | `prepare_url/1` (move/duplicate from `window_test.exs`) |
| T-WIN-02 | `handle_cast :hide` / `:set_title` with nil frame |
| T-WIN-03 | `handle_cast :close_window` all branches (taskbar nil / set, on_close hide/quit) |
| T-WIN-04 | `@tag :wx` — minimal Window `start_link` + cast `:close_window` alive |

### Backend contracts — `test/desktop/backend/*_test.exs` (new)

| File | ID | Scope |
|------|-----|-------|
| `browser_test.exs` | T-BRW-* | Every Window/Content/Notification/Media/System callback — no raise |
| `json_test.exs` | T-JSN-* | Mock transport: new/open/connect/loadURL handle shapes |
| `wx_test.exs` | T-WX-* | `@tag :wx` — open, connect, is_shown?, attach |

### Bridge — `bridge_codec_test.exs` (extend) + `bridge_transport_test.exs` (new)

| ID | Test |
|----|------|
| T-BRG-01 | Codec round-trip (existing) |
| T-BRG-02 | Mock RPC response |
| T-BRG-03 | `subscribe_events` delivers queued ref=0 event |
| T-BRG-04 | Connect cast JSON shape |
| T-BRG-05 | Callback ref=1 invokes registered fun |

### Menu / Fallback — `menu_adapter_test.exs`, `fallback_test.exs` (new)

| ID | Test |
|----|------|
| T-MENU-01 | Browser adapter menubar nil, set_icon ok |
| T-MENU-02 | Json adapter minimal menubar DOM |
| T-FALL-01 | `webview_load` with nil webview does not raise |

### L0 guard — `test/support/guard_boolean_ops.exs` (new)

| ID | Test |
|----|------|
| L0-GUARD | Fail if `lib/desktop/window.ex` matches `if\s+\w+\s+and\s+(not\s+)?` where LHS is not `==` / `!=` |

---

## CI recommendation

```bash
mix test.fast          # default PR check
xvfb-run mix test.wx   # required for wx-tagged tests
mix test.guard         # optional static regression guard
```

Exclude: `test/mix/tasks/desktop.install_test.exs` (pre-existing).

---

## Implementation order

1. Infrastructure (`desktop_case`, `wx_case`, mix aliases)
2. Regression tests (BOOL, CONN) — highest ROI
3. Env + language_code
4. Window lifecycle + close_window
5. Backend contract smoke tests
6. Bridge transport
7. Menu + Fallback
8. L0 guard + AGENTS.md CI docs
