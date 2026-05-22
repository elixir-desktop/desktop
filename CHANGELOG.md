# Changelog

## Changes in 1.6 (unreleased)

- Internal `Desktop.Platform` layer with domain behaviours (Window, Content, Notification, Media, System) and backends (`Desktop.Backend.Wx`, `Desktop.Backend.Json`, `Desktop.Backend.Browser`)
- Bridge JSON/TCP transport embedded as `Desktop.Bridge.Transport` (legacy `[module, method, args]` wire format preserved for native hosts)
- Removed separate `{:wx, hex: :bridge}` dependency on Android/iOS; mobile builds use `Desktop.Backend.Json` directly
- Optional `config :desktop, :backend, :auto | :wx | :json | :browser` override
- Menu adapters: `Desktop.Menu.Adapter.Json` and `Desktop.Menu.Adapter.Browser`
- Public `Desktop.*` APIs unchanged (`Desktop.Window`, `Desktop.Env`, `Desktop.Menu`, etc.)

## Changes in 1.5

- Support for iOS hibernation and wakeup
- `mix desktop.check_toolchain` verifies running Erlang/OTP (major) and Elixir against `.tool-versions`; docs describe mise and asdf equally for Linux contributors

## Changes in 1.4

- Support for `~H` sigil with `<item onclick={...}>`
- Changed Menu XML escaping to LiveView automatic escaping logic from LiveView HTML tokenizer & engine.
- Removed `Menu.escape_attribute/1` and `Menu.escape/1`
- Dropped `:xmerl` dependency

## Changes in 1.3

- Added `Env.subscribe/1` to listen to MacOS application events such as `{:open_url, url}` when a url associated with the application is clicked.
- Using (experimental) dbus support to render the systray icon on linux
- Added `Menu.escape_attribute/1`
- Added `Window.url/1`
- Added `Window.hide/1` and `Window.is_hidden/`
- Fixed MacOS "Quit" menu item
- Other fixes

## Changes in 1.2

- Removed the bleeding edge notices because Erlang OTP 24 is now broadly available for Windows/Linux/MacOS
- Added build support for iOS devices
- Moved most phoenix dependencies out to allow using Phoenix 1.6 / esbuild
