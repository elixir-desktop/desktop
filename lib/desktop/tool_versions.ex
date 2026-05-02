defmodule Desktop.ToolVersions do
  @moduledoc false

  @doc """
  Parses a `.tool-versions` file body (asdf/mise format).

  Returns a map with optional string values for `:erlang` and `:elixir` keys
  (raw version fields from the file, e.g. `"26.2.5.5 system"`, `"1.19.1-otp-26"`).
  """
  @spec parse(String.t()) :: %{optional(:erlang) => String.t(), optional(:elixir) => String.t()}
  def parse(content) when is_binary(content) do
    content
    |> String.split("\n")
    |> Enum.reduce(%{}, &parse_line/2)
  end

  defp parse_line(line, acc) do
    line = String.trim(line)

    cond do
      line == "" ->
        acc

      String.starts_with?(line, "#") ->
        acc

      true ->
        case Regex.run(~r/^(elixir|erlang)\s+(.+)$/, line) do
          [_, "elixir", rest] ->
            Map.put(acc, :elixir, String.trim(rest))

          [_, "erlang", rest] ->
            Map.put(acc, :erlang, String.trim(rest))

          _ ->
            acc
        end
    end
  end
end
