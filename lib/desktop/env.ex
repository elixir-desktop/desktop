defmodule Desktop.Env do
  alias Desktop.Env
  use GenServer

  defstruct [:wx_env, :wx, :map, :waiters]

  @doc false
  @spec start_link() :: :ignore | {:error, any} | {:ok, pid}
  def start_link() do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
  end

  @doc false
  def init(_arg) do
    wx = :wx.new([])
    {:ok, %Env{wx_env: :wx.get_env(), wx: wx, map: %{}, waiters: %{}}}
  end

  def handle_call(:wx_env, _from, d = %Env{wx_env: env}) do
    {:reply, env, d}
  end

  def handle_call(:wx, _from, d = %Env{wx: wx}) do
    {:reply, wx, d}
  end

  def handle_call({:get, key, default}, _from, d = %Env{map: map}) do
    {:reply, Map.get(map, key, default), d}
  end

  def handle_call({:put, key, value}, _from, d = %Env{map: map, waiters: waiters}) do
    {froms, waiters} = Map.pop(waiters, key, [])
    Enum.each(froms, fn from -> GenServer.reply(from, value) end)
    {:reply, Map.get(map, key), %Env{d | map: Map.put(map, key, value), waiters: waiters}}
  end

  def handle_call({:await, key}, from, d = %Env{map: map, waiters: waiters}) do
    if Map.has_key?(map, key) do
      {:reply, Map.get(map, key), d}
    else
      waiters = Map.update(waiters, key, [from], fn froms -> [from | froms] end)
      {:noreply, %Env{d | waiters: waiters}}
    end
  end

  def handle_call({:connect, object, command, callback, id}, _from, d) do
    opts = [{:callback, fn _, _ -> callback.() end}]
    opts = if id == nil, do: opts, else: [{:id, id} | opts]
    ret = :wxMenu.connect(object, command, opts)
    {:reply, ret, d}
  end

  def wx() do
    GenServer.call(__MODULE__, :wx)
  end

  def wx_env() do
    GenServer.call(__MODULE__, :wx_env)
  end

  def connect(object, command, callback, id \\ nil) do
    GenServer.call(__MODULE__, {:connect, object, command, callback, id})
  end

  def put(key, value) do
    GenServer.call(__MODULE__, {:put, key, value})
  end

  def get(key, default \\ nil) do
    GenServer.call(__MODULE__, {:get, key, default})
  end

  def await(key) do
    GenServer.call(__MODULE__, {:await, key})
  end
end
