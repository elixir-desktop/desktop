# Desktop

[![Module Version](https://img.shields.io/hexpm/v/desktop.svg)](https://hex.pm/packages/desktop)
[![Hex Docs](https://img.shields.io/badge/hex-docs-lightgreen.svg)](https://hexdocs.pm/desktop/)
[![Total Download](https://img.shields.io/hexpm/dt/desktop.svg)](https://hex.pm/packages/desktop)
[![License](https://img.shields.io/hexpm/l/desktop.svg)](https://github.com/elixir-desktop/desktop/blob/master/LICENSE.md)
[![Last Updated](https://img.shields.io/github/last-commit/elixir-desktop/desktop.svg)](https://github.com/elixir-desktop/desktop/commits/master)


Building native-like Elixir apps for Windows, MacOS, Linux, iOS and Android using Phoenix LiveView!

## Changes in 1.5
- Support for iOS hibernation and wakeup

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

## Example

Checkout the Todo sample application for a starting point. https://github.com/elixir-desktop/desktop-example-app

![MacOS build](https://raw.githubusercontent.com/elixir-desktop/desktop-example-app/main/nodeploy/macos_todo.png "MacOS build")

## Getting Started

Check out the [Getting your Environment Ready Guide](./guides/getting_started.md) and [Your first Desktop App](./guides/your_first_app.md)

## Status / Roadmap

1. Run elixir-desktop on MacOS/Windows/Android ✅
1. Run elixir-desktop on Android/iOS ✅
1. Package elixir-desktop based apps for Android/iOS ✅ (see example-app repo for iOS and Android)
1. Package elixir-desktop based apps in installers for MacOS/Windows/Linux
1. Update the desktop example app built installers for MacOS/Windows/Linux using GitHub actions
1. Use a repo of pre-compiled Erlang binaries for Android/iOS/MacOS/Windows/Linux when building Apps, so one call of `mix desktop.deploy` will create installer for all platforms.
1. Integrate Code Signing for Windows/MacOS
1. Build a GitHub actions based auto-update flow

## Contributing and Raising Issues

Please checkout the github repo at https://github.com/elixir-desktop/desktop and raise any issues. For questions and comments best to use the Elixir forum: https://elixirforum.com/t/elixir-desktop-android-window-macos-linux-liveview-apps-with-elixir/ or contact me on Slack https://elixir-lang.slack.com/archives/C02429KJJCX

All suggestions are welcome!

Cheers!

## Copyright and License

Copyright (c) 2021 Dominic Letz

This work is free. You can redistribute it and/or modify it under the terms of the MIT License. See the [LICENSE.md](./LICENSE.md) file for more details.
