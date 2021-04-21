defmodule Desktop do
  @moduledoc """
  This is the documentation for the Desktop project.

  By default, Desktop applications depend on the following packages:

    * [Phoenix](https://hexdocs.pm/phoenix) - the Phoenix web framework

    * [Phoenix LiveView](https://hexdocs.pm/phoenix_live_view) - real-time
      user experience

    * [Sqlite3 Ecto](https://github.com/elixir-sqlite/ecto_sqlite3) - local
      database

  To get started, see our [guides](introduction.html).
  """
  use Application

  @doc false
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
