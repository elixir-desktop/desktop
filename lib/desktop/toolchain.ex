defmodule Desktop.Toolchain do
  @moduledoc false

  @doc """
  Verifies that the running Erlang/OTP and Elixir versions match `.tool-versions` in
  `project_root` when that file declares `erlang` and/or `elixir` entries.

  Returns `{:ok, :no_tool_versions}` if the file is missing, or `{:ok, :verified}` on success.

  `opts` may override runtime versions for testing:

  * `:otp_release` — string from `System.otp_release/0` (major OTP, e.g. `"26"`)
  * `:elixir_version` — string from `System.version/0` (e.g. `"1.19.1"`)
  """
  @spec verify(String.t(), keyword()) ::
          {:ok, :no_tool_versions | :verified} | {:error, [String.t()]}
  def verify(project_root, opts \\ []) do
    otp = Keyword.get_lazy(opts, :otp_release, &System.otp_release/0)
    elixir = Keyword.get_lazy(opts, :elixir_version, &System.version/0)

    case Desktop.ToolVersions.read_from_dir(project_root) do
      {:error, :enoent} ->
        {:ok, :no_tool_versions}

      {:ok, tools} ->
        errors =
          []
          |> maybe_check_otp(tools, otp)
          |> maybe_check_elixir(tools, elixir)

        if errors == [] do
          {:ok, :verified}
        else
          {:error, Enum.reverse(errors)}
        end
    end
  end

  defp maybe_check_otp(errors, tools, otp) do
    case Map.get(tools, "erlang") do
      nil ->
        errors

      declared ->
        expected = otp_major_from_declared(declared)

        if expected && expected != otp do
          [
            "Erlang/OTP mismatch: .tool-versions requests OTP #{expected} (from #{declared}), but this shell is OTP #{otp}."
            | errors
          ]
        else
          errors
        end
    end
  end

  defp maybe_check_elixir(errors, tools, elixir) do
    case Map.get(tools, "elixir") do
      nil ->
        errors

      declared ->
        expected = elixir_semver_from_declared(declared)
        running = elixir_semver_from_declared(elixir)

        if expected && running && expected != running do
          [
            "Elixir mismatch: .tool-versions requests #{expected} (from #{declared}), but this shell is #{running}."
            | errors
          ]
        else
          errors
        end
    end
  end

  defp otp_major_from_declared(declared) do
    declared
    |> String.split(".", parts: 2)
    |> List.first()
  end

  defp elixir_semver_from_declared(declared) do
    declared
    |> String.split("-", parts: 2)
    |> List.first()
  end
end
