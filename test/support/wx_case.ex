defmodule Desktop.Test.WxCase do
  @moduledoc false

  defmacro __using__(_opts) do
    quote do
      use ExUnit.Case

      @moduletag :wx

      setup _tags do
        previous = Application.get_env(:desktop, :backend, :auto)
        Application.put_env(:desktop, :backend, :wx)

        {:ok, _} = Application.ensure_all_started(:desktop)
        Desktop.Env.wx_use_env()

        on_exit(fn ->
          Application.put_env(:desktop, :backend, previous)
        end)

        :ok
      end
    end
  end
end
