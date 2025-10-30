defmodule Mix.Tasks.Desktop.Install do
  @shortdoc "Add Elixir Desktop support to a project"

  @moduledoc """
  #{@shortdoc}
  """

  use Igniter.Mix.Task

  @impl Igniter.Mix.Task
  def info(_argv, _composing_task) do
    %Igniter.Mix.Task.Info{
      # Groups allow for overlapping arguments for tasks by the same author
      # See the generators guide for more.
      group: :desktop
    }
  end

  @impl Igniter.Mix.Task
  def igniter(igniter) do
    app = Igniter.Project.Application.app_name(igniter)
    endpoint = Igniter.Libs.Phoenix.web_module_name(igniter, "Endpoint")
    menu = Igniter.Project.Module.module_name(igniter, "Menu")
    menubar = Igniter.Project.Module.module_name(igniter, "MenuBar")
    gettext = Igniter.Libs.Phoenix.web_module_name(igniter, "Gettext")
    main_window = Igniter.Project.Module.module_name(igniter, MainWindow)

    igniter
    |> Igniter.compose_task("igniter.add", ["desktop"])
    |> add_menu_bar(menubar, gettext, endpoint)
    |> add_menu(menu, gettext, app, main_window)
    |> Igniter.Project.Application.add_new_child(
      {
        Desktop.Window,
        {:code,
         quote do
           [
             app: unquote(app),
             id: unquote(Igniter.Project.Module.module_name(igniter, MainWindow)),
             title: unquote(to_string(app)),
             size: {600, 500},
             # icon: "icon.png", # TODO: ship an example taskbar icon here
             menubar: unquote(menubar),
             icon_menu: unquote(menu),
             url: &unquote(endpoint).url/0
           ]
         end}
      },
      after: [endpoint]
    )

    # TODO: Add runtime_tools observer if available, or optionall add the dependency
  end

  defp add_menu_bar(igniter, menubar, gettext, endpoint) do
    Igniter.Project.Module.find_and_update_or_create_module(
      igniter,
      menubar,
      """
        @moduledoc \"""
          Menu bar that is shown as part of the main window on Windows/Linux. In
          MacOS this menu bar appears at the very top of the screen.
        \"""
        use Gettext, backend: #{inspect(gettext)}
        use Desktop.Menu
        alias Desktop.Window

        @impl true
        def render(assigns) do
          ~H\"""
          <menubar>
          <menu label={gettext "File"}>
              <item onclick="quit">{gettext "Quit"}</item>
          </menu>
          <menu label={gettext "Extra"}>
              <item onclick="browser">{gettext "Open Browser"}</item>
          </menu>
          </menubar>
          \"""
        end

        @impl true
        def handle_event("quit", menu) do
          Window.quit()
          {:noreply, menu}
        end

        def handle_event("browser", menu) do
          Window.prepare_url(#{inspect(endpoint)}.url())
          |> :wx_misc.launchDefaultBrowser()

          {:noreply, menu}
        end

        @impl true
        def handle_info(_, menu) do
          {:noreply, menu}
        end

        @impl true
        def mount(menu) do
          {:ok, menu}
        end
      """,
      fn zipper -> {:ok, zipper} end
    )
  end

  defp add_menu(igniter, menu, gettext, app, main_window) do
    Igniter.Project.Module.find_and_update_or_create_module(
      igniter,
      menu,
      """
        @moduledoc \"""
        Menu that is shown when a user clicks on the taskbar icon of the #{app}
        \"""
        use Gettext, backend: #{inspect(gettext)}
        use Desktop.Menu

        @impl true
        def render(assigns) do
          ~H\"""
          <menu>
            <item onclick="edit">{gettext "Open"}</item>
            <hr/>
            <item onclick="quit">{gettext "Quit"}</item>
          </menu>
          \"""
        end

        @impl true
        def handle_event(command, menu) do
          case command do
            <<"quit">> -> Desktop.Window.quit()
            <<"edit">> -> Desktop.Window.show(#{inspect(main_window)})
          end

          {:noreply, menu}
        end

        @impl true
        def handle_info(_, menu) do
          {:noreply, menu}
        end

        @impl true
        def mount(menu) do
          {:ok, menu}
        end
      """,
      fn zipper -> {:ok, zipper} end
    )
  end
end
