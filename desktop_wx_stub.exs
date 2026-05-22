# Standalone entry for regenerating src/desktop_wx.erl (also invoked from mix.exs).
[{Desktop.WxStub, _}] = Code.compile_file(Path.join(__DIR__, "lib/desktop/wx/stub.ex"))
Desktop.WxStub.write!()
