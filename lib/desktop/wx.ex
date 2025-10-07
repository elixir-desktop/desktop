# TODO: less stringy metaprogramming
defmodule Desktop.Wx do
  @moduledoc """
  Elixir version of the constants found in the wx.hrl file, reduced to what is needed in this sample only.
  """
  @constants ~w(
    ID_ANY ID_EXIT DEFAULT_FRAME_STYLE NO_BORDER EXPAND HORIZONTAL VERTICAL
    ITEM_SEPARATOR ITEM_NORMAL ITEM_CHECK ITEM_RADIO
    ICON_WARNING ICON_ERROR ICON_QUESTION ICON_INFORMATION
    MAJOR_VERSION MINOR_VERSION RELEASE_NUMBER IMAGE_QUALITY_HIGH )

  gets =
    Enum.sort(@constants)
    |> Enum.map(fn const -> "get(wx#{const}) -> ?wx#{const}" end)
    |> Enum.join(";\n  ")

  File.write!(
    __DIR__ <> "/../../src/desktop_wx.erl",
    """
      -module(desktop_wx).
      -include_lib("wx/include/wx.hrl").
      -export([get/1]).

      #{gets}.
    """
  )

  @constants
  |> Enum.map(&String.to_atom("wx" <> &1))
  |> Enum.each(fn prefixed_constant ->
    def unquote(prefixed_constant)(), do: :desktop_wx.get(unquote(prefixed_constant))
  end)
end
