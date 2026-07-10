defmodule Desktop.Backend.Null do
  @moduledoc false

  def wx_call(module, method, args \\ []) do
    if wx_enabled?() and Code.ensure_loaded?(module) and
         function_exported?(module, method, length(args)) do
      apply(module, method, args)
    end
  end

  def wx_enabled? do
    System.get_env("NO_WX") == nil
  end

  def module?(module) do
    Code.ensure_compiled(module) == {:module, module}
  end
end
