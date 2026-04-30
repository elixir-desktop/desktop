defmodule Desktop.ToolVersions do
  @moduledoc false

  @doc """
  Parses a `.tool-versions` file body into a map of tool name => declared version string.

  Names are normalized to lowercase. Lines starting with `#` and blank lines are ignored.
  For lines with more than two whitespace-separated fields (e.g. `erlang 26.2.5.5 system`),
  the version is the second field only.
  """
  @spec parse(String.t()) :: %{optional(String.t()) => String.t()}
  def parse(content) when is_binary(content) do
    content
    |> String.split("\n")
    |> Enum.flat_map(&parse_line/1)
    |> Map.new()
  end

  defp parse_line(line) do
    line = String.trim(line)

    cond do
      line == "" or String.starts_with?(line, "#") ->
        []

      true ->
        case String.split(line, ~r/\s+/, parts: 3) do
          [tool, version] -> [{String.downcase(tool), version}]
          [tool, version, _rest] -> [{String.downcase(tool), version}]
          _ -> []
        end
    end
  end

  @doc """
  Reads `.tool-versions` from `directory` and parses it. Returns `{:ok, map}` or
  `{:error, :enoent}` when the file is missing.
  """
  @spec read_from_dir(String.t()) :: {:ok, map()} | {:error, :enoent}
  def read_from_dir(directory) do
    path = Path.join(directory, ".tool-versions")

    case File.read(path) do
      {:ok, body} -> {:ok, parse(body)}
      {:error, :enoent} -> {:error, :enoent}
      {:error, _} -> {:error, :enoent}
    end
  end
end
