# Your first Desktop App

> If you prefer to start with a working Desktop sample application, please refer to [elixir-desktop/desktop-example-app](https://github.com/elixir-desktop/desktop-example-app).

## 1. Create a demo Phoenix LiveView app

```
$ mix phx.new demo --no-ecto --no-dashboard --no-mailer
```

After running this command, a demo Phoenix LiveView app will be created:

- OTP app name is `:demo`
- root module name is `Demo`
- web root module name is `DemoWeb`

## 2. Change above app as a Desktop app

### 2.1 Add required dependency

Add `desktop` package to the dependencies in `mix.exs`:

```elixir
def deps do
  [
    # ...
    {:desktop, github: "elixir-desktop/desktop"}
  ]
end
```

### 2.2 Tweak existing Phoenix endpoint

```elixir
defmodule DemoWeb.Endpoint do
  # Step 1: Change the endpoint as a `Desktop` endpoint.
  use Desktop.Endpoint, otp_app: :demo

  # ...

  # Step 2: Add `Desktop.Auth` plug to ensure that only requests from
  # the Desktop app's WebView are allowed.
  plug Desktop.Auth

  plug YourAppWeb.Router
end
```

### 2.3 Add child specification for opening WebView window

```elixir
defmodule Demo.Application do
  # ...

  def start(_type, _args) do
    children = [
      # ...

      DemoWeb.Endpoint,
      {Desktop.Window,
       [
         app: :demo,
         id: DemoWindow,
         url: &DemoWeb.Endpoint.url/0
       ]}
    ]

    # ...
  end
end
```

### 2.4 Configure the Desktop endpoint

> You can configure it in `config/dev.exs` or `config/runtime.exs`.

```
config :demo, DemoWeb.Endpoint,
  # Step 1: set the port number to 0, so the port can be chosen automatically.
  http: [ip: {127, 0, 0, 1}, port: 0],

  # Step 2: start endpoint automatically, so we have no need to start it manually.
  server: true,
  # ...
```

## 3. Extra Steps (optional)

### 3.1 Localization support

For localization and to autodetect the desktop language, desktop provides a helper function for that. You can call it before starting the app:

```elixir
defmodule Demo.Application do
  # ...

  def start(_type, _args) do
    Desktop.identify_default_locale(DemoWeb.Gettext)

    children = [
      # ...
    ]

    # ...
  end
end
```

## 4. Run it

```
$ mix run
```

The Desktop app should be visible now!
