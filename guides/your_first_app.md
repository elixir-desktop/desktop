# Your first Desktop App

If you prefer to start with a working Desktop sample application, please refer to https://github.com/elixir-desktop/desktop-example-app

To convert a barebones Phoenix Live View example to a Desktop Application you will need to add this to your application:

1. Add the `desktop` dependency to your project's dependencies in `mix.exs`

    ```elixir
    def deps do
    [
        {:desktop, "~> 1.5"}
    ]
    end
    ```

1. Add a Desktop.Window child to your supervision tree on startup. E.g. in `application.ex`

    ```elixir
    children = [
        # After your other children
        # Starting Desktop.Windows
        {Desktop.Window,
         [
            app: :your_app,
            id: YourAppWindow,
            url: &YourAppWeb.Endpoint.url/0
         ]}
    ]
    Supervisor.start_link()
    ```

	Note: you still need to start your phoenix Endpoint, add `Desktop.Window` in _addition_ to your `Desktop.Window`

1. In `endpoint.ex` call `use Desktop.Endpoint` **instead of** `use Phoenix.Endpoint`

    ```elixir
    defmodule YourAppWeb.Endpoint do
      use Desktop.Endpoint, otp_app: :your_app
      ...
    end
    ```

    1. Consider authenticating requests

    Adding the `Desktop.Auth` plug to your endpoint ensures that only request from the app's webview are allowed.

    ```elixir
    defmodule YourAppWeb.Endpoint do
      ...
      plug Desktop.Auth
      plug YourAppWeb.Router
    end
    ```

1. In your application configuration (by default in `config/dev.exs` and `config/runtime.exs)` ensure http is configured and the port is set to `0` so it's chosen automatically, as well as setting `server: true` so that your Phoenix Endpoint starts automatically (without having to explicitly call `mix phx.server`):

    ```elixir
    # Configures the endpoint
    config :your_app, YourAppWeb.Endpoint,
        http: [ip: {127, 0, 0, 1}, port: 0],
        server: true,
    ...
    ```

1. For localization and to autodetect the desktop language (optional), add the detection hook to your application startup. E.g. in `application.ex`:


    ```elixir
      def start(_type, args) do
        Desktop.identify_default_locale(YourWebApp.Gettext)

        children = [
            ...
        ]

        ...
      end
    ```

1. Run your application with `mix run`

  The desktop application window should now be visible!
