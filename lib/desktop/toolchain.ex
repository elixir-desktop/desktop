defmodule Desktop.Toolchain do
  @moduledoc false

  alias Desktop.ToolVersions

  @doc """
  Verifies that running OTP major and Elixir version match entries parsed from `.tool-versions`.

  `requirements` is the map returned by `Desktop.ToolVersions.parse/1`.

  Optional `otp_release` and `elixir_version` override `System` values (for testing).

  Returns `:ok` or `{:error, message}` where `message` is human-readable.
  """
  @spec verify(map(), keyword()) :: :ok | {:error, String.t()}
  def verify(requirements, opts \\ []) when is_map(requirements) do
    otp_actual = Keyword.get_lazy(opts, :otp_release, fn -> System.otp_release() end)
    elixir_actual = Keyword.get_lazy(opts, :elixir_version, fn -> System.version() end)

    errors =
      []
      |> maybe_check_erlang(Map.get(requirements, :erlang), otp_actual)
      |> maybe_check_elixir(Map.get(requirements, :elixir), elixir_actual)

    case errors do
      [] -> :ok
      msgs -> {:error, Enum.join(msgs, "\n")}
    end
  end

  defp maybe_check_erlang(acc, nil, _otp_actual), do: acc

  defp maybe_check_erlang(acc, raw, otp_actual) when is_binary(raw) do
    expected_major = erlang_major(raw)

    case Integer.parse(to_string(otp_actual)) do
      {actual_major, _} when actual_major == expected_major ->
        acc

      {actual_major, _} ->
        [
          "Erlang/OTP major mismatch: running OTP #{actual_major}, `.tool-versions` expects OTP #{expected_major} (from erlang #{inspect(raw)})."
          | acc
        ]

      :error ->
        ["Could not parse running OTP release #{inspect(otp_actual)}." | acc]
    end
  end

  defp maybe_check_elixir(acc, nil, _elixir_actual), do: acc

  defp maybe_check_elixir(acc, raw, elixir_actual) when is_binary(raw) do
    expected_base = elixir_base_version(raw)

    case Version.parse(expected_base) do
      {:ok, expected_ver} ->
        case Version.parse(elixir_actual) do
          {:ok, actual_ver} ->
            if Version.compare(actual_ver, expected_ver) == :eq do
              acc
            else
              [
                "Elixir version mismatch: running #{elixir_actual}, `.tool-versions` expects #{expected_base} (from elixir #{inspect(raw)})."
                | acc
              ]
            end

          :error ->
            ["Could not parse running Elixir version #{inspect(elixir_actual)}." | acc]
        end

      :error ->
        ["Could not parse Elixir version in `.tool-versions`: #{inspect(raw)}." | acc]
    end
  end

  @doc false
  def erlang_major(raw) when is_binary(raw) do
    raw
    |> String.split()
    |> hd()
    |> String.split(".")
    |> hd()
    |> String.to_integer()
  end

  @doc false
  def elixir_base_version(raw) when is_binary(raw) do
    case Regex.run(~r/^(\d+\.\d+\.\d+)/, raw) do
      [_, base] ->
        base

      nil ->
        raw
        |> String.split("-")
        |> hd()
    end
  end

  @doc """
  Loads `.tool-versions` from `path`, parses it, and verifies the toolchain.
  """
  @spec verify_file(String.t(), keyword()) :: :ok | {:error, String.t()}
  def verify_file(path, opts \\ []) do
    case File.read(path) do
      {:ok, content} ->
        content |> ToolVersions.parse() |> verify(opts)

      {:error, reason} ->
        {:error, "Could not read #{inspect(path)}: #{inspect(reason)}"}
    end
  end
end
