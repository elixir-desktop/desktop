defmodule Desktop do
  use Application

  def start(:normal, []) do
    child = %{
      id: Desktop.Env,
      start: {Desktop.Env, :start_link, []}
    }

    Supervisor.start_link([child], strategy: :one_for_one, name: Desktop.Sup)
  end

  @doc false
  @spec start_link() :: :ignore | {:error, any} | {:ok, pid}
  def start_link() do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
  end
end
