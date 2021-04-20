defmodule Desktop.MixProject do
  use Mix.Project

  def project do
    [
      app: :desktop,
      name: "Desktop",
      version: "1.0.0",
      source_url: "https://github.com/dominicletz/desktop",
      description: """
      Write Desktop Apps with Elixir.
      """,
      elixir: "~> 1.10",
      elixirc_paths: elixirc_paths(Mix.env()),
      compilers: [:phoenix, :gettext] ++ Mix.compilers(),
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  # Specifies which paths to compile per environment.
  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      mod: {Desktop, []},
      extra_applications: [
        :logger,
        :wx,
        :ssl,
        :crypto,
        :sasl,
        :tools,
        :inets,
        :eex,
        :xmerl
      ]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:oncrash, "~> 0.1"},

      # phoenix stuff
      {:phoenix, "~> 1.5.7"},
      {:phoenix_live_view, "~> 0.15"},
      {:phoenix_html, "~> 2.11"},
      {:phoenix_live_reload, "~> 1.2"},
      {:gettext, "~> 0.11"},
      {:plug_cowboy, "~> 2.0"},
      {:jason, "~> 1.0"}
    ]
  end
end
