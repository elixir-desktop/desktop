defmodule DesktopWindowTest do
  use ExUnit.Case

  test "prepare_url" do
    assert Desktop.Window.prepare_url(nil) == nil

    expected = "/some/?k=" <> Desktop.Auth.login_key()
    assert Desktop.Window.prepare_url("/some/?k=123") == expected
    assert Desktop.Window.prepare_url("/some/") == expected

    expected = "/some/?bla=something_cool&k=" <> Desktop.Auth.login_key()
    assert Desktop.Window.prepare_url("/some/?bla=something_cool") == expected
    assert Desktop.Window.prepare_url("/some/?bla=something_cool&k=xx") == expected
  end
end
