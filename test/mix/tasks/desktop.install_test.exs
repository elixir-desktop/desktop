defmodule Mix.Tasks.Desktop.InstallTest do
  use ExUnit.Case, async: true
  import Igniter.Test

  test "it installs desktop dependency and window" do
    phx_test_project(app_name: :my_app)
    |> Igniter.compose_task("desktop.install", [])
    |> assert_has_patch("mix.exs", """
      + | {:desktop,
    """)
    |> assert_has_patch("lib/my_app/application.ex", """
     - | MyAppWeb.Endpoint
     + | MyAppWeb.Endpoint,
     + | {Desktop.Window,
     + |  [
     + |    app: :my_app,
     + |    id: MyApp.MainWindow,
     + |    title: "my_app",
     + |    size: {600, 500},
     + |    menubar: MyApp.MenuBar,
     + |    icon_menu: MyApp.Menu,
     + |    url: &MyAppWeb.Endpoint.url/0
     + |  ]}
    """)
    |> Igniter.Test.assert_creates("lib/my_app/menu.ex")
    |> Igniter.Test.assert_creates("lib/my_app/menu_bar.ex")
  end
end
