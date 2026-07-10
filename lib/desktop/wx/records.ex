defmodule Desktop.Wx.Records do
  @moduledoc false
  # wx event records for pattern matching in Window and Menu.Adapter.Wx.
  # On MIX_TARGET=android|ios, use minimal stubs (wx.hrl is not on erlc's code path).

  @host_build System.get_env("MIX_TARGET") in [nil, "host"]

  @wx_hrl (if @host_build do
             case :code.lib_dir(:wx) do
               path when is_list(path) ->
                 path = Path.join([List.to_string(path), "include", "wx.hrl"])
                 if File.exists?(path), do: path

               _ ->
                 nil
             end
           end)

  if @wx_hrl do
    require Record

    for tag <- [:wx, :wxCommand, :wxClose, :wxMenu] do
      Record.defrecord(tag, Record.extract(tag, from: @wx_hrl))
    end
  else
    require Record

    Record.defrecord(:wx, id: nil, obj: nil, userData: nil, event: nil)
    Record.defrecord(:wxCommand, type: nil, cmdString: nil, commandInt: nil, extraLong: nil)
    Record.defrecord(:wxClose, type: nil)
    Record.defrecord(:wxMenu, type: nil)
  end
end
