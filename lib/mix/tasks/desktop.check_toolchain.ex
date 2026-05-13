defmodule Mix.Tasks.Desktop.CheckToolchain do
  @shortdoc "Checks running Erlang/OTP and Elixir against `.tool-versions`"

  @moduledoc """
  #{@shortdoc}

  Compares the **currently running** BEAM (`System.otp_release/0`, `System.version/0`)
  to `erlang` and `elixir` lines in `.tool-versions`. It does not invoke mise, asdf, or
  other installers—activate your toolchain however you prefer, then run this task to fail fast.

  ## Examples

      mix desktop.check_toolchain

  """

  use Mix.Task

  @impl Mix.Task
  def run(_argv) do
    root = Mix.Project.config()[:root] || File.cwd!()
    path = Path.join(root, ".tool-versions")

    unless File.exists?(path) do
      Mix.shell().error("No `.tool-versions` found at #{path}.")
      exit({:shutdown, 1})
    end

    case Desktop.Toolchain.verify_file(path) do
      :ok ->
        Mix.shell().info("Toolchain matches `.tool-versions`.")

      {:error, msg} ->
        Mix.shell().error(msg)
        exit({:shutdown, 1})
    end
  end
end
