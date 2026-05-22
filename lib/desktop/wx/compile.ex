defmodule Desktop.Wx.Compile do
  @moduledoc false

  @constants ~w(
    ID_ANY ID_EXIT DEFAULT_FRAME_STYLE NO_BORDER EXPAND HORIZONTAL VERTICAL
    ITEM_SEPARATOR ITEM_NORMAL ITEM_CHECK ITEM_RADIO
    ICON_WARNING ICON_ERROR ICON_QUESTION ICON_INFORMATION
    MAJOR_VERSION MINOR_VERSION RELEASE_NUMBER IMAGE_QUALITY_HIGH
  )

  @fallback %{
    wxID_ANY: -1,
    wxID_EXIT: 5006,
    wxDEFAULT_FRAME_STYLE: 541_072_960,
    wxNO_BORDER: 2_097_152,
    wxEXPAND: 8192,
    wxHORIZONTAL: 4,
    wxVERTICAL: 8,
    wxITEM_SEPARATOR: -1,
    wxITEM_NORMAL: 0,
    wxITEM_CHECK: 1,
    wxITEM_RADIO: 2,
    wxICON_WARNING: 256,
    wxICON_ERROR: 512,
    wxICON_QUESTION: 1024,
    wxICON_INFORMATION: 2048,
    wxMAJOR_VERSION: 0,
    wxMINOR_VERSION: 0,
    wxRELEASE_NUMBER: 0,
    wxIMAGE_QUALITY_HIGH: 192
  }

  defmacro __before_compile__(_env) do
    write_stub_file!()
    constant_defs()
  end

  @doc false
  def write_stub_file! do
    root = Path.expand("../../..", __DIR__)
    stub = Path.join(root, "desktop_wx_stub.exs")

    if File.exists?(stub) do
      Code.require_file(stub)
      Desktop.WxStub.write!(root)
    end
  end

  def wx_available? do
    host_target?() and wx_headers_exist?()
  end

  defp host_target? do
    System.get_env("MIX_TARGET") in [nil, "host"]
  end

  defp wx_headers_exist? do
    case :code.lib_dir(:wx) do
      path when is_list(path) ->
        File.exists?(Path.join([List.to_string(path), "include", "wx.hrl"]))

      _ ->
        false
    end
  end

  defp constant_defs do
    if host_target?() and wx_headers_exist?() do
      constant_defs_from_erlang()
    else
      constant_defs_from_fallback()
    end
  end

  defp constant_defs_from_erlang do
    for const <- @constants do
      name = String.to_atom("wx" <> const)

      quote do
        def unquote(name)(), do: :desktop_wx.get(unquote(name))
      end
    end
  end

  defp constant_defs_from_fallback do
    for const <- @constants do
      name = String.to_atom("wx" <> const)
      value = Map.fetch!(@fallback, name)

      quote do
        def unquote(name)(), do: unquote(value)
      end
    end
  end
end
