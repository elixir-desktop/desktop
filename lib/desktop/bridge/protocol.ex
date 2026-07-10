defmodule Desktop.Bridge.Protocol do
  @moduledoc false

  alias Desktop.Bridge.Transport

  @doc """
  Legacy bridge RPC: `[module, method, args]` JSON encoding.
  """
  def call(module, method, args \\ []) do
    Transport.bridge_call(module, method, args)
  end

  def new(module, args \\ []) do
    Transport.bridge_call(module, :new, args)
  end

  def connect(module, object, event, opts \\ []) do
    Transport.bridge_call(module, :connect, [object, event, opts])
  end

  def destroy(module, object) do
    Transport.bridge_call(module, :destroy, [object])
  end
end
