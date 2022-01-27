defmodule Desktop.MixProject do
  use Mix.Project

  @version "1.4.0"
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
      compilers: Mix.compilers(),
      aliases: aliases(),
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
        :crypto,
        :eex,
        :inets,
        :logger,
        :sasl,
        :ssl,
        :tools
        | extra_applications(Mix.target())
      ]
    ]
  end

  def extra_applications(:host) do
    [:wx]
  end

  def extra_applications(_mobile) do
    []
  end

  defp aliases() do
    [
      lint: [
        "compile --warnings-as-errors",
        "format --check-formatted",
        "credo --ignore refactor",
        "dialyzer"
      ]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps() do
    desktop = [
      {:ex_doc, "~> 0.25", only: :dev, runtime: false},
      {:oncrash, "~> 0.1"},
      {:debouncer, "~> 0.1"},
      {:credo, "~> 1.5", only: [:dev, :test], runtime: false},
      {:dialyxir, "~> 1.0", only: [:dev], runtime: false},

      # DBus SNI
      {:ex_sni, "~> 0.2"},
      # {:ex_sni, path: "../ex_sni"},

      # Phoenix & Plug
      {:phoenix, "> 1.0.0"},
      {:phoenix_live_view, "> 0.15.0"},
      {:plug, "> 1.0.0"},
      {:gettext, "> 0.10.0"}
    ]

    if Mix.target() in [:android, :ios] do
      desktop ++ [{:wx, "~> 1.0", hex: :bridge, targets: [:android, :ios]}]
    else
      desktop
    end
  end

  defp docs do
    [
      extra_section: "GUIDES",
      extras: [
        "LICENSE.md": [title: "License"],
        "README.md": [title: "Overview"],
        "guides/getting_started.md": [title: "Getting your Environment Ready"],
        "guides/your_first_app.md": [title: "Your first Desktop App"]
      ],
      main: "readme",
      source_ref: "v#{@version}",
      source_url: @url,
      formatters: ["html"]
    ]
  end

  defp package do
    [
      maintainers: ["Dominic Letz"],
      licenses: ["MIT"],
      links: %{github: @url},
      files: ~w(src lib LICENSE.md mix.exs README.md)
    ]
  end
end
