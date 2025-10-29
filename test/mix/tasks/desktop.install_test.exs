defmodule Mix.Tasks.Desktop.InstallTest do
  use ExUnit.Case, async: true
  import Igniter.Test

  test "it installs desktop dependency" do
    test_project()
    |> Igniter.compose_task("desktop.install", [])
    |> assert_has_patch("mix.exs", """
      + | {:desktop,
    """)
  end
end
