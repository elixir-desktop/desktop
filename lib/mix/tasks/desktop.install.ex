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

    igniter
    |> Igniter.compose_task("igniter.add", ["desktop"])
    |> Igniter.Project.Application.add_new_child(
      {
        Desktop.Window,
        [
          app: app,
          id: Igniter.Project.Module.module_name(igniter, MainWindow),
          # FIXME: configurable
          title: to_string(app),
          size: {600, 500},
          icon: "icon.png",
          menubar: Igniter.Project.Module.module_name(igniter, MenuBar),
          icon_menu: Igniter.Project.Module.module_name(igniter, Menu),
          url: {endpoint, :url, []}
        ]
      },
      after: [endpoint]
    )

    # TODO: detect and warn if the project assumes pgsql
    # TODO: create MyApp.MenuBar
  end
end
