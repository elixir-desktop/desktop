defmodule Desktop.Env do
  alias Desktop.Env
  use GenServer

  defstruct [:wx_env, :wx, :map, :waiters, :windows, :sni]

  @doc false
  @spec start_link() :: :ignore | {:error, any} | {:ok, pid}
  def start_link() do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
  end

  @doc false
  @impl true
  def init(_arg) do
    wx = :wx.new([])
    sni = init_sni()

    Desktop.Fallback.wx_subscribe()
    {:ok, %Env{wx_env: :wx.get_env(), wx: wx, map: %{}, waiters: %{}, windows: [], sni: sni}}
  end

  @impl true
  def handle_call(:wx_env, _from, d = %Env{wx_env: env}) do
    {:reply, env, d}
  end

  @impl true
  def handle_call(:wx, _from, d = %Env{wx: wx}) do
    {:reply, wx, d}
  end

  @impl true
  def handle_call(:sni, _from, %Env{sni: sni} = state) do
    {:reply, sni, state}
  end

  @impl true
  def handle_call({:get, key, default}, _from, d = %Env{map: map}) do
    {:reply, Map.get(map, key, default), d}
  end

  @impl true
  def handle_call({:pop, key, default}, _from, d = %Env{map: map}) do
    {value, map} = Map.pop(map, key, default)
    {:reply, value, %Env{d | map: map}}
  end

  @impl true
  def handle_call({:put, key, value}, _from, d = %Env{map: map, waiters: waiters}) do
    {froms, waiters} = Map.pop(waiters, key, [])
    Enum.each(froms, fn from -> GenServer.reply(from, value) end)
    {:reply, Map.get(map, key), %Env{d | map: Map.put(map, key, value), waiters: waiters}}
  end

  @impl true
  def handle_call({:await, key}, from, d = %Env{map: map, waiters: waiters}) do
    if Map.has_key?(map, key) do
      {:reply, Map.get(map, key), d}
    else
      waiters = Map.update(waiters, key, [from], fn froms -> [from | froms] end)
      {:noreply, %Env{d | waiters: waiters}}
    end
  end

  @impl true
  def handle_call({:connect, object, command, callback, id}, _from, d) do
    opts = [{:callback, fn _, _ -> callback.() end}]
    opts = if id == nil, do: opts, else: [{:id, id} | opts]
    ret = :wxMenu.connect(object, command, opts)
    {:reply, ret, d}
  end

  @impl true
  def handle_cast({:register_window, window}, state = %Env{windows: windows}) do
    Process.monitor(window)
    {:noreply, %Env{state | windows: [window | windows]}}
  end

  @impl true
  def handle_info({:reopen_app, []}, state = %Env{windows: windows}) do
    # Handling MacOS event when the app icon is clicked again
    case windows do
      [window | _] ->
        # Avoiding constant reopen loops
        Debouncer.immediate2({Desktop, :reopen}, fn -> Desktop.Window.show(window) end, 500)

      [] ->
        :nothing
    end

    {:noreply, state}
  end

  @impl true
  def handle_info({mac_event, list}, state = %Env{}) when is_list(list) do
    IO.puts("Ignoring event: #{mac_event} #{inspect(list)}")
    {:noreply, state}
  end

  @impl true
  def handle_info({:DOWN, _ref, :process, pid, _reason}, state = %Env{windows: windows}) do
    {:noreply, %Env{state | windows: windows -- [pid]}}
  end

  def sni() do
    GenServer.call(__MODULE__, :sni)
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

  def pop(key, default \\ nil) do
    GenServer.call(__MODULE__, {:pop, key, default})
  end

  def await(key) do
    GenServer.call(__MODULE__, {:await, key})
  end

  defp init_sni() do
    if Desktop.OS.type() == Linux do
      case ExSni.start_link() do
        {:ok, pid} ->
          if ExSni.is_supported?(pid) do
            pid
          else
            ExSni.close(pid)
            nil
          end

        _ ->
          nil
      end
    end
  end
end
