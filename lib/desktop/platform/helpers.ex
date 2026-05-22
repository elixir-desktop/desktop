defmodule Desktop.Platform.Helpers do
  @moduledoc false

  @doc """
  Ensures the current process can use wx APIs via the active backend.

  Safe on all backends (no-op when `Desktop.Env` is down or `wx_env` is nil).
  """
  def with_wx_env(fun) when is_function(fun, 0) do
    Desktop.Env.wx_use_env()
    fun.()
  end
end
