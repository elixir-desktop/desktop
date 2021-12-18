defmodule Desktop.Wx do
  @moduledoc """
  Elixir version of the constants found in the wx.hrl file, reduced to what is needed in this sample only.
  """
  @constants ~w(
    ID_ANY ID_EXIT DEFAULT_FRAME_STYLE NO_BORDER EXPAND HORIZONTAL
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

  for wx_constant <- @constants do
    Module.eval_quoted(
      __MODULE__,
      Code.string_to_quoted("""
        def wx#{wx_constant}, do: :desktop_wx.get(:wx#{wx_constant})
      """)
    )
  end
end
