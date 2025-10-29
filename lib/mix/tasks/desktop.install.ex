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
      group: :desktop,
      # *other* dependencies to add
      # i.e `{:foo, "~> 2.0"}`
      adds_deps: [],
      # *other* dependencies to add and call their associated installers, if they exist
      # i.e `{:foo, "~> 2.0"}`
      installs: [],
      # An example invocation
      # example: __MODULE__.Docs.example(),
      # A list of environments that this should be installed in.
      only: nil,
      # a list of positional arguments, i.e `[:file]`
      positional: [],
      # Other tasks your task composes using `Igniter.compose_task`, passing in the CLI argv
      # This ensures your option schema includes options from nested tasks
      composes: [],
      # `OptionParser` schema
      schema: [],
      # Default values for the options in the `schema`
      defaults: [],
      # CLI aliases
      aliases: [],
      # A list of options in the schema that are required
      required: []
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
    |> Igniter.Project.Module.create_module(menubar, """
      @moduledoc """
        Menu bar that is shown as part of the main window on Windows/Linux. In
        MacOS this menu bar appears at the very top of the screen.
      \"""
      use Gettext, backend: #{gettext}
      use Desktop.Menu
      alias Desktop.Window

      def render(assigns) do
        ~H"""
        <menubar>
        <menu label={gettext "File"}>
            <item onclick="quit">{gettext "Quit"}</item>
        </menu>
        <menu label={gettext "Extra"}>
            <item onclick="observer">{gettext "Show Observer"}</item>
            <item onclick="browser">{gettext "Open Browser"}</item>
        </menu>
        </menubar>
        \"""
      end

      def handle_event("quit", menu) do
        Window.quit()
        {:noreply, menu}
      end

      def handle_event("observer", menu) do
        :observer.start()
        {:noreply, menu}
      end

      def handle_event("browser", menu) do
        Window.prepare_url(#{endpoint}.url())
        |> :wx_misc.launchDefaultBrowser()

        {:noreply, menu}
      end

      def mount(menu) do
        {:ok, menu}
      end
    """)
    |> Igniter.Project.Module.create_module(menu, """
      @moduledoc """
      Menu that is shown when a user clicks on the taskbar icon of the #{app}
      \"""
      use Gettext, backend: #{gettext}
      use Desktop.Menu

      def render(assigns) do
        ~H"""
        <menu>
          <item onclick="edit">{gettext "Open"}</item>
          <hr/>
          <item onclick="quit">{gettext "Quit"}</item>
        </menu>
        \"""
      end

      def handle_event(command, menu) do
        case command do
          <<"quit">> -> Desktop.Window.quit()
          <<"edit">> -> Desktop.Window.show(#{main_window})
        end

        {:noreply, menu}
      end

      def mount(menu) do
        {:ok, menu}
      end
    """)
    |> Igniter.Project.Application.add_new_child(
      {
        Desktop.Window,
        [
          app: app,
          id: Igniter.Project.Module.module_name(igniter, MainWindow),
          title: to_string(app),
          size: {600, 500},
          # icon: "icon.png", # TODO: ship an example taskbar icon here
          menubar: menubar,
          icon_menu: menu,
          url: fn -> apply(endpoint, :url, []) end
        ]
      },
      after: [endpoint]
    )
    # TODO: detect and warn if the project assumes pgsql
  end
end
