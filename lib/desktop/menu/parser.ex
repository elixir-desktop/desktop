defmodule Desktop.Menu.Parser do
  @moduledoc false
  require Record
  require Logger
  alias Desktop.Menu.HTMLTokenizer
  alias Desktop.Menu.HTMLTokenizer.ParseError

  def parse(data) when is_tuple(data) or is_map(data) do
    Phoenix.HTML.Safe.to_iodata(data)
    |> parse()
  end

  def parse(string) when is_list(string) or is_binary(string) do
    string = :unicode.characters_to_binary(string)

    try do
      HTMLTokenizer.tokenize(string, "generated", 0, [])
      # 'simple-form' is what xmerl documentation calls a tree of tuples:
      # simple_form() := {tag, attributes = %{key => value}, content = [simple_form()]} | binary()
      |> simple_form([])
      |> case do
        [{:menu, _, _} = menu | _] ->
          menu

        [{:menubar, _, _} = menubar | _] ->
          menubar

        [] ->
          """
            Expected "<menu>" or "<menubar>" root tag. Document seems empty
          """
          |> error()

        _other ->
          """
          Expected "<menu>" or "<menubar>" root tag.
          """
          |> error()
      end
    rescue
      error in ParseError ->
        filename = Path.absname("parse_error.xml")
        File.write(filename, string)
        IO.warn(error.description)
        Logger.error("Failed to parse document. Dumped #{filename} #{inspect(error)}")

        []
    catch
      :exit, error ->
        filename = Path.absname("parse_error.xml")
        File.write(filename, string)
        Logger.error("Failed to parse document. Dumped #{filename} #{inspect(error)}")

        []
    end
  end

  @known_tags ["hr", "item", "menu", "menubar"]
  defp simple_form([{:tag_open, tag, attrs, meta} | rest], content) do
    if tag not in @known_tags do
      """
        Found unexpected tag "<#{tag}>" on line #{meta.line} col #{meta.column}.
      
        Only the following tags are supported:
      
        <#{Enum.join(@known_tags, "> <")}>
      """
      |> error(meta)
    end

    attrs =
      attrs
      |> Enum.map(fn
        {key, {:string, value, _meta}} -> {String.to_atom(key), html_decode(value)}
        {key, true} -> {String.to_atom(key), "true"}
        {key, false} -> {String.to_atom(key), "false"}
        {key, nil} -> {String.to_atom(key), "true"}
      end)
      |> Map.new()

    atom = String.to_atom(tag)

    if Map.get(meta, :self_close, false) or atom == :hr do
      sub_content = check_content(atom, [], meta)
      simple_form(rest, content ++ [{atom, attrs, sub_content}])
    else
      case simple_form(rest, []) do
        {sub_content, rest, ^tag} ->
          sub_content = check_content(atom, sub_content, meta)
          simple_form(rest, content ++ [{atom, attrs, sub_content}])

        {_sub_content, _rest, other_tag} ->
          """
            Expected closing tag "</#{tag}>" but found "</#{other_tag}>" for tag
            starting at line: #{meta.line} column: #{meta.column}
          """
          |> error(meta)

        list when is_list(list) ->
          """
            Missing closing tag "</#{tag}>" for tag
            starting at line: #{meta.line} column: #{meta.column}
          """
          |> error(meta)
      end
    end
  end

  defp simple_form([{:tag_close, tag, _meta} | rest], content) do
    {content, rest, tag}
  end

  defp simple_form([{:text, text} | rest], content) do
    simple_form(rest, content ++ [String.trim_trailing(text)])
  end

  defp simple_form([], content) do
    content
  end

  defp check_content(:item, content, meta) do
    if Enum.any?(content, fn c -> not is_binary(c) end) do
      """
        "<item>" tag at line: #{meta.line} column: #{meta.column} can
        only have text content.
      """
      |> error(meta)
    end

    [html_decode(Enum.join(content))]
  end

  defp check_content(tag, content, meta) do
    mistake = Enum.find(content, fn c -> is_binary(c) and String.trim(c) != "" end)

    if mistake != nil do
      """
        "<#{tag}>" tag at line: #{meta.line} column: #{meta.column} can
        not contain text content ("#{mistake}").
      """
      |> error(meta)
    end

    Enum.filter(content, &(not is_binary(&1)))
  end

  defp error(message, meta \\ %{line: 0, column: 0}) do
    raise ParseError, file: "unknown", line: meta.line, column: meta.column, description: message
  end

  defp html_decode(binary) do
    html_decode(binary, [])
    |> Enum.reverse()
    |> :erlang.iolist_to_binary()
  end

  escapes = [
    {?<, "&lt;"},
    {?>, "&gt;"},
    {?&, "&amp;"},
    {?", "&quot;"},
    {?', "&#39;"}
  ]

  for {insert, match} <- escapes do
    defp html_decode(<<unquote(match)::binary, rest::binary>>, acc) do
      html_decode(rest, [unquote(insert) | acc])
    end
  end

  defp html_decode(<<char, rest::bits>>, acc) do
    html_decode(rest, [char | acc])
  end

  defp html_decode(<<>>, acc) do
    acc
  end
end
