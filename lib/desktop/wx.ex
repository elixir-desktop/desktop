defmodule Desktop.Wx do
  @moduledoc """
  wxWidgets constants used by `desktop`.

  When OTP is built with the `:wx` application, values are taken from `wx/include/wx.hrl`
  via the `desktop_wx` Erlang module. Otherwise compile-time fallbacks are used so the
  library can build on mobile targets and minimal OTP installs without wxWidgets.
  """
  @before_compile Desktop.Wx.Compile

  @doc false
  def wx_headers_available?, do: Desktop.Wx.Compile.wx_available?()
end
