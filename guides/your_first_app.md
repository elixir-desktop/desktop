# Your first Desktop App

If you prefer to start with a working Desktop sample application, please refer to https://github.com/elixir-desktop/desktop-example-app

To convert a barebones Phoenix Live View example to a Desktop Application you will need to add this to your application:

1. Add the `desktop` dependency to your project's dependencies in `mix.exs`

    ```elixir
    def deps do
    [
        {:desktop, "~> 1.4"}
    ]
    end
    ```

1. Add a Desktop.Window child to your supervision tree on startup. E.g. in `application.ex`

    ```
    children = [{
        # After your other children
        # Starting Desktop.Windows
        Desktop.Window,
        [
            app: :your_app,
            id: YourAppWindow,
            url: &YourAppWeb.Endpoint.url/0
        ]
    }]
    Supervisor.start_link()
    ```

1. In `endpoint.ex` call `use Desktop.Endpoint` instead of `use Phoenix.Endpoint`

    ```
    defmodule YourAppWeb.Endpoint do
    use Desktop.Endpoint, otp_app: :your_app
    ```

1. In `config.exs` ensure http is configured and the port is set to `0` so it's chosen automatically

    ```
    # Configures the endpoint
    config :your_app, YourAppWeb.Endpoint,
        http: [ip: {127, 0, 0, 1}, port: 0],
        server: true,
    ...
    ```

1. For localization and to autodetect the desktop language (optional), add the detection hook to your application startup. E.g. in `application.ex`:


    ```
    def start(_type, args) do 
        Desktop.identify_default_locale(YourWebApp.Gettext)

        children = [
            ...
    ```

