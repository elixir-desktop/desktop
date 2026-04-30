defmodule Mix.Tasks.Desktop.CheckToolchain do
  @shortdoc "Checks Erlang/OTP and Elixir against .tool-versions in the project"

  @moduledoc """
  #{@shortdoc}

  This task is tooling-agnostic: it does not invoke asdf, mise, or kerl. It only
  compares the **currently active** `elixir` and `erlang` in your shell against the
  versions declared in `.tool-versions` at the project root (next to `mix.exs`).

  Use it from CI scripts or Android build wrappers to fail fast with a clear message
  when the wrong runtime is active.

  ## Examples

      mix desktop.check_toolchain
  """

  use Mix.Task

  @impl Mix.Task
  def run(_argv) do
    root = project_root()

    case Desktop.Toolchain.verify(root) do
      {:ok, :no_tool_versions} ->
        Mix.shell().info("No .tool-versions found; skipping version check.")

      {:ok, :verified} ->
        Mix.shell().info(".tool-versions matches the active Erlang/OTP and Elixir.")

      {:error, messages} ->
        for msg <- messages, do: Mix.shell().error(msg)

        unless toolchain_hint_irrelevant?(messages) do
          Mix.shell().error("""
          Activate the versions in .tool-versions for this project, for example:
            mise install && mise exec -- mix desktop.check_toolchain
          or:
            asdf install && asdf exec mix desktop.check_toolchain
          """)
        end

        System.halt(1)
    end
  end

  defp project_root do
    if mix_project?() do
      Mix.Project.project_file() |> Path.dirname()
    else
      File.cwd!()
    end
  end

  defp mix_project? do
    Mix.Project.get() != nil
  end

  defp toolchain_hint_irrelevant?(messages) do
    Enum.any?(messages, &match?("Could not read .tool-versions:" <> _, &1))
  end
end
