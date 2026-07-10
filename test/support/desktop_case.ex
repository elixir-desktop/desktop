defmodule Desktop.Test.DesktopCase do
  @moduledoc false

  defmacro __using__(_opts) do
    quote do
      use ExUnit.Case

      import Desktop.Test.DesktopCase
    end
  end

  def fake_frame(id \\ 1) do
    {:wx_ref, id, :wxFrame, []}
  end

  def fake_webview(id \\ 2) do
    {:wx_ref, id, :wxWebView, []}
  end

  def minimal_window(overrides \\ []) do
    struct!(
      %Desktop.Window{
        module: nil,
        taskbar: nil,
        frame: nil,
        id: :test_window,
        notifications: %{},
        webview: nil,
        home_url: nil,
        last_url: nil,
        title: "Test",
        on_close: :quit
      },
      overrides
    )
  end

  def with_backend(backend, fun) when is_function(fun, 0) do
    previous = Application.get_env(:desktop, :backend, :auto)

    Application.put_env(:desktop, :backend, backend)

    try do
      fun.()
    after
      Application.put_env(:desktop, :backend, previous)
    end
  end

  def with_env(key, value, fun) when is_function(fun, 0) do
    previous = System.get_env(key)

    if value == nil do
      System.delete_env(key)
    else
      System.put_env(key, value)
    end

    try do
      fun.()
    after
      if previous == nil do
        System.delete_env(key)
      else
        System.put_env(key, previous)
      end
    end
  end
end
