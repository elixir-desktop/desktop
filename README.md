# Desktop

[![Module Version](https://img.shields.io/hexpm/v/desktop.svg)](https://hex.pm/packages/desktop)
[![Hex Docs](https://img.shields.io/badge/hex-docs-lightgreen.svg)](https://hexdocs.pm/desktop/)
[![Total Download](https://img.shields.io/hexpm/dt/desktop.svg)](https://hex.pm/packages/desktop)
[![License](https://img.shields.io/hexpm/l/desktop.svg)](https://github.com/elixir-desktop/desktop/blob/master/LICENSE.md)
[![Last Updated](https://img.shields.io/github/last-commit/elixir-desktop/desktop.svg)](https://github.com/elixir-desktop/desktop/commits/master)

Building native-like Elixir apps for Windows, MacOS, Linux, iOS and Android using Phoenix LiveView!

## Example

Checkout [the example app](https://github.com/elixir-desktop/desktop-example-app) for a starting point.

![MacOS build](https://raw.githubusercontent.com/elixir-desktop/desktop-example-app/main/nodeploy/macos_todo.png 'MacOS build')

## Getting Started

Check out the [Getting your Environment Ready Guide](./guides/getting_started.md) and [Your first Desktop App](./guides/your_first_desktop_app.md)

## Status / Roadmap

1. ✅ Run elixir-desktop on Windows/MacOS/Linux
2. ✅ Run elixir-desktop on iOS/Android
3. ✅ Package elixir-desktop based apps for iOS/Android (see example-app repo for iOS and Android)
4. Package elixir-desktop based apps in installers for Windows/MacOS/Linux
5. Update the desktop example app built installers for Windows/MacOS/Linux using GitHub actions
6. Use a repo of pre-compiled Erlang binaries for Windows/MacOS/Linux/iOS/Android when building Apps, so one call of `mix desktop.deploy` will create installer for all platforms.
7. Integrate Code Signing for Windows/MacOS
8. Build a GitHub actions based auto-update flow

## Contributing and Raising Issues

- Please checkout [the GitHub repo](https://github.com/elixir-desktop/desktop) and raise any issue.
- For questions and comments best to use the [Elixir forum](https://elixirforum.com/t/elixir-desktop-android-window-macos-linux-liveview-apps-with-elixir/) or contact me on [Slack](https://elixir-lang.slack.com/archives/C02429KJJCX).

All suggestions are welcome!

Cheers!

## Copyright and License

Copyright (c) 2021 Dominic Letz

This work is free. You can redistribute it and/or modify it under the terms of the MIT License. See the [LICENSE.md](./LICENSE.md) file for more details.
