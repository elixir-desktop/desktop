defmodule Desktop.MixProject do
  use Mix.Project

  @version "1.0.0"
  @url "https://github.com/elixir-desktop/desktop"

  def project do
    [
      app: :desktop,
      name: "Desktop",
      version: @version,
      source_url: @url,
      description: """
      Write Desktop Apps with Elixir.
      """,
      elixir: "~> 1.10",
      elixirc_paths: elixirc_paths(Mix.env()),
      compilers: [:phoenix, :gettext] ++ Mix.compilers(),
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      docs: docs(),
      package: package()
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
      {:ex_doc, "~> 0.22", only: :dev, runtime: false},
      {:oncrash, "~> 0.1"},
      {:debouncer, "~> 0.1"},

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

  defp docs do
    [
      main: "introduction",
      source_ref: "v#{@version}",
      source_url: @url,
      extra_section: "GUIDES",
      extras: [
        "guides/introduction.md"
      ]
    ]
  end

  defp package do
    [
      maintainers: ["Dominic Letz"],
      licenses: ["MIT"],
      links: %{github: @url},
      files:
        ~w(src lib priv) ++
          ~w(LICENSE.md mix.exs README.md)
    ]
  end
end
