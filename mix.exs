defmodule Desktop.MixProject do
  use Mix.Project

  @description """
  Building native-like Elixir apps for Windows, MacOS, Linux, iOS and Android using Phoenix LiveView!
  """
  @version "1.5.3"
  @url "https://github.com/elixir-desktop/desktop"

  def cli do
    [
      preferred_envs: [
        test: :test,
        "test.fast": :test,
        "test.wx": :test
      ]
    ]
  end

  def project do
    ensure_desktop_wx_erl!()

    [
      app: :desktop,
      name: "Desktop",
      version: @version,
      source_url: @url,
      description: @description,
      elixir: "~> 1.11",
      elixirc_paths: elixirc_paths(Mix.env()),
      erl_src_paths: erl_src_paths(),
      compilers: Mix.compilers(),
      aliases: aliases(),
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      docs: docs(),
      package: package(),
      dialyzer: [
        plt_add_apps: [:ex_unit, :mix],
        plt_file: {:no_warn, "priv/plts/dialyzer.plt"}
      ]
    ]
  end

  defp ensure_desktop_wx_erl! do
    script = Path.join(__DIR__, "desktop_wx_stub.exs")
    {_, 0} = System.cmd("elixir", [script], env: System.get_env())
    :ok
  end

  # Specifies which paths to compile per environment.
  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  # Compile src/desktop_wx.erl on host only (uses wx.hrl when available, else stub).
  # Android/iOS set MIX_TARGET — skip Erlang (erlc has no :wx on the code path).
  defp erl_src_paths do
    if System.get_env("MIX_TARGET") in [nil, "host"], do: ["src"], else: []
  end

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
    # Only include `:wx` when the OTP it would be loaded on actually has the
    # `:wx` OTP application available. Without this guard, `mix release`
    # aborts with "Could not find application :wx" when the build host's
    # Erlang/OTP was configured `--without-wx` (e.g. on the macOS installer
    # CI now that the workflow drops the custom wxWidgets build). The Erlang
    # source file `src/desktop_wx.erl` already adapts to missing wx headers
    # via `desktop_wx_stub.exs`, so a host build without `:wx` simply
    # compiles the stub backend.
    #
    # NOTE: `:code.lib_dir/1` is unreliable here because it consults Erlang's
    # `NameDb` ETS table which can be empty by the time `extra_applications/1`
    # is evaluated under `mix deps.compile` — the previous deps' `compile.all`
    # prunes the code path and forgets the apps it never listed. Check the
    # filesystem directly instead, mirroring what `desktop_wx_stub.exs`
    # already does via `wx_headers_resolvable?/0`. The `with` clause guards
    # `wx_dir` against `nil` because `Enum.find/2` returns `nil` when no
    # `wx-*` directory exists under the OTP root (the case on `--without-wx`
    # builds), and an unbound pattern would otherwise let the body run with
    # `wx_dir = nil` and crash `Path.join/1`.
    if wx_app_on_disk?() do
      [:wx]
    else
      []
    end
  end

  def extra_applications(_mobile) do
    []
  end

  defp wx_app_on_disk? do
    root = List.to_string(:code.root_dir())

    with {:ok, entries} <- File.ls(Path.join(root, "lib")),
         wx_dir when is_binary(wx_dir) <-
           Enum.find(entries, &String.starts_with?(&1, "wx-")) do
      File.exists?(Path.join([root, "lib", wx_dir, "include", "wx.hrl"]))
    else
      _ -> false
    end
  end

  defp aliases() do
    [
      "test.fast": ["test --exclude wx"],
      "test.wx": ["test --only wx"],
      "test.guard": [
        "run test/support/guard_boolean_ops.exs",
        "run test/support/guard_platform_abstraction.exs"
      ],
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
      {:phoenix, "> 1.7.10"},
      {:phoenix_live_view, "> 1.0.0"},
      {:plug, "> 1.0.0"},
      {:gettext, "> 0.10.0"},
      {:igniter, "~> 0.6", optional: true},
      {:jason, "~> 1.2"}
    ]

    desktop
  end

  defp docs do
    [
      extra_section: "GUIDES",
      extras: [
        "LICENSE.md": [title: "License"],
        "README.md": [title: "Overview"],
        "CHANGELOG.md": [title: "Changelog"],
        "guides/getting_started.md": [title: "Getting your Environment Ready"],
        "guides/your_first_desktop_app.md": [title: "Your first Desktop App"],
        "guides/faq.md": [title: "FAQ"]
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
      files: ~w(src lib LICENSE.md mix.exs README.md desktop_wx_stub.exs)
    ]
  end
end
