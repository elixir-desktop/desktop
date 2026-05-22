# Frequently Asked Questions (FAQ)

## How do I choose the platform backend?

All UI operations go through `Desktop.Platform`, which delegates to a single **backend module**. Public APIs (`Desktop.Window`, `Desktop.Menu`, etc.) are unchanged — only the implementation underneath varies.

### Automatic (default)

```elixir
config :desktop, :backend, :auto
```

`Desktop.Platform.backend/0` picks:

| Condition | Backend |
|---|---|
| `Mix.target()` is `:android` or `:ios`, or `Desktop.OS.mobile?/0` | `Desktop.Backend.Json` |
| `NO_WX` is set, or OTP `:wx` is not available | `Desktop.Backend.Browser` |
| Otherwise | `Desktop.Backend.Wx` |

### Explicit config

In `config/config.exs` (or environment-specific config):

```elixir
config :desktop, :backend, :wx      # native wxWidgets window + webview
config :desktop, :backend, :json    # JSON/TCP bridge (mobile native host)
config :desktop, :backend, :browser # OS default browser, no native window
```

For a custom implementation, set the backend to a module that implements the `Desktop.Platform.Window`, `Content`, `Notification`, `Media`, `System`, and `Menu` behaviour callbacks:

```elixir
config :desktop, :backend, MyApp.DesktopBackend
```

Restart the app after changing backend config — the router reads `Application.get_env(:desktop, :backend, :auto)` at runtime.

### Environment variables

- **`NO_WX=1`** — with `:auto`, forces `Desktop.Backend.Browser`. Useful for headless servers, CI without a display, or local development when wxWidgets is not installed.
- **`BRIDGE_PORT`** — TCP port where the native host listens for `Desktop.Backend.Json` RPC (see mobile bridge below). Use `0` for the in-process mock transport (tests).

### Capabilities

Inspect what the active backend supports:

```elixir
Desktop.Platform.backend()       # e.g. Desktop.Backend.Wx
Desktop.Platform.capabilities()  # %{window: true, content: :webview, ...}
```

| Backend | `window` | `content` | `menu` |
|---|---|---|---|
| Wx | yes | `:webview` | `:wx` or `:dbus` (Linux SNI) |
| Json | yes | `:native` (host webview) | `:native` |
| Browser | no | `:os_browser` | `:none` |

On Linux with DBus SNI available, Wx may use `:dbus` for the taskbar menu instead of in-window wx menus.

## How does the mobile (Android/iOS) bridge work?

On mobile targets, `desktop` uses `Desktop.Backend.Json` instead of OTP `:wx`. The Elixir side speaks the legacy JSON protocol over TCP to your native host app. Set `BRIDGE_PORT` to the port your host app listens on. Transport is built in as `Desktop.Bridge.Transport` — the separate `bridge` hex package is no longer required.

Override explicitly if needed:

```elixir
config :desktop, :backend, :json
```

## Can I compile without the `:wx` OTP application?

Yes. Android/iOS release builds set `MIX_TARGET=android` (or `ios`). `desktop` then:

- Regenerates **`src/desktop_wx.erl`** via `desktop_wx_stub.exs` when Mix loads the project: on **host** with `:wx` in OTP it uses `wx/include/wx.hrl`; on **android/ios** (or without wx headers) it writes header-free integer fallbacks. Erlang compilation of that file is skipped when `MIX_TARGET` is not `host`.
- Uses **`Desktop.Wx`** integer fallbacks instead of `wx.hrl` macros.
- Uses **`Desktop.Wx.Records`** stubs instead of `Record.extract(..., from_lib: "wx/include/wx.hrl")`.

On a host-only build without `:wx` in OTP, the same fallbacks apply when `wx.hrl` is missing.

Runtime uses `Desktop.Backend.Json` on mobile; you do not need wx installed to compile or release.

## How do I run without wxWidgets?

Set `NO_WX=1` to use `Desktop.Backend.Browser` under `:auto`: URLs open in the OS default browser and window/menu APIs degrade gracefully (notifications are logged).

To **test** the Wx backend on headless Linux, keep wx enabled and use a virtual display instead:

```bash
xvfb-run -a mix phx.server
```

Library contributors can run `mix test.fast` (no wx), `xvfb-run -a mix test.wx`, and `mix test.guard` — see `AGENTS.md` in the repo root.

## How do I release and distribute my Desktop application?

### Creating an Installer

To create a distributable installer for your Desktop application, use the following command:

```bash
mix desktop.installer
```

This command will create platform-specific installers for Windows, macOS, and Linux that you can distribute to your users.

### About `mix release`

While `mix release` is a standard Elixir command for creating releases, it's not the recommended approach for Desktop applications. The `mix release` command creates a standalone Erlang release, but it doesn't include the platform-specific packaging and UI components needed for a Desktop application.

### Distribution

After running `mix desktop.installer`, you'll find the installer files in your project's build directory. These are the files you should distribute to your users:

- **Windows**: `.exe` installer file
- **macOS**: `.dmg` or `.app` bundle
- **Linux**: `.AppImage`, `.deb`, or `.rpm` package

Users can simply download and run these installers to install your application on their system.

### Getting Started

If you're new to Elixir Desktop:

1. Follow the [Getting your Environment Ready Guide](getting_started.md) to set up your development environment
2. Create your first app with the [Your first Desktop App Guide](your_first_desktop_app.md)
3. When ready to distribute, run `mix desktop.installer` to create platform-specific installers
