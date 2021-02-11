defmodule Desktop do
  use Application
  use GenServer

  defstruct [:wx_env, :wx]

  def start(:normal, []) do
    child = %{
      id: __MODULE__,
      start: {__MODULE__, :start_link, []}
    }

    Supervisor.start_link([child], strategy: :one_for_one, name: Desktop.Sup)
  end

  @doc false
  @spec start_link() :: :ignore | {:error, any} | {:ok, pid}
  def start_link() do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
  end

  @doc false
  def init(_arg) do
    __MODULE__ = :ets.new(__MODULE__, [:named_table, :public])
    wx = :wx.new([])
    {:ok, %Desktop{wx_env: :wx.get_env(), wx: wx}}
  end

  def handle_call(:wx_env, _from, d = %Desktop{wx_env: env}) do
    {:reply, env, d}
  end

  def handle_call(:wx, _from, d = %Desktop{wx: wx}) do
    {:reply, wx, d}
  end

  def wx() do
    GenServer.call(__MODULE__, :wx)
  end

  def wx_env() do
    GenServer.call(__MODULE__, :wx_env)
  end
end
