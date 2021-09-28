defmodule Desktop.Menu.Parser do
  @moduledoc """
  Parser that converts a XML string into a Menu DOM struct
  """
  require Record
  require Logger

  for tag <- [:xmlElement, :xmlAttribute, :xmlText] do
    Record.defrecordp(tag, Record.extract(tag, from_lib: "xmerl/include/xmerl.hrl"))
  end

  def escape(string) do
    unicode(string)
    |> :xmerl_lib.export_text()
    |> List.to_string()
  end

  def parse({:safe, string}) do
    parse(string)
  end

  def parse(string) do
    string = unicode(string)

    try do
      {xml, []} = :xmerl_scan.string(string, encoding: :ref)

      # 'simple-form' is what xmerl documentation calls a tree of tuples:
      # {tag, attributes = %{key => value}, content = []}
      simple_form(xmlElement(xml, :content))
    catch
      :exit, error ->
        Logger.error("Failed to parse document #{inspect(error)}")

        []
    end
  end

  defp unicode(string) do
    :unicode.characters_to_list(string)
  end

  defp simple_form([]) do
    []
  end

  defp simple_form([element | rest]) do
    case simple_form(element) do
      :skip -> simple_form(rest)
      ret -> [ret | simple_form(rest)]
    end
  end

  defp simple_form(xmlElement(name: name, attributes: attr, content: children)) do
    attr =
      Enum.map(attr, fn xmlAttribute(name: tname, value: value) ->
        {tname, List.to_string(value)}
      end)
      |> Map.new()

    {name, attr, simple_form(children)}
  end

  defp simple_form(xmlText(value: value)) do
    List.to_string(value)
    |> String.trim_trailing()
    |> case do
      "" -> :skip
      other -> other
    end
  end
end
