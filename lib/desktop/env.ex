defmodule Desktop.Env do
  @moduledoc """
    Env holds any needed :wx / Desktop application state. Currently
    it keeps track of
      * The open Dekstop.Window(s),
      * OS Application events (such as when a file is dragged on the application icon)
      * The :wx environment
      * The dbus connection (sni) on linux
  
    Also it has a global connect() method to allow binding of :wx event callbacks using
    this long lived process as reference.
  """
  alias Desktop.Env
  use GenServer
  require Logger

  defstruct [:wx_env, :wx, :map, :waiters, :windows, :sni, :events, :subs]

  @doc false
  @spec start_link() :: :ignore | {:error, any} | {:ok, pid}
  def start_link() do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
  end

  @doc false
  @impl true
  def init(_arg) do
    wx = :wx.new([])
    Desktop.Fallback.wx_subscribe()

    {:ok,
     %Env{
       wx_env: :wx.get_env(),
       wx: wx,
       map: %{},
       waiters: %{},
       windows: [],
       sni: :not_initialized,
       events: [],
       subs: []
     }}
  end

  @impl true
  def handle_call({:subscribe, pid}, _from, d = %Env{events: events, subs: subs}) do
    for e <- events do
      send(pid, e)
    end

    Process.monitor(pid)
    {:reply, :ok, %Env{d | subs: [pid | subs], events: []}}
  end

  def handle_call(:wx_env, _from, d = %Env{wx_env: env}) do
    {:reply, env, d}
  end

  def handle_call(:wx, _from, d = %Env{wx: wx}) do
    {:reply, wx, d}
  end

  def handle_call(:sni, _from, state = %Env{sni: :not_initialized}) do
    sni = init_sni()
    {:reply, sni, %Env{state | sni: sni}}
  end

  def handle_call(:sni, _from, state = %Env{sni: sni}) do
    {:reply, sni, state}
  end

  def handle_call({:get, key, default}, _from, d = %Env{map: map}) do
    {:reply, Map.get(map, key, default), d}
  end

  def handle_call({:pop, key, default}, _from, d = %Env{map: map}) do
    {value, map} = Map.pop(map, key, default)
    {:reply, value, %Env{d | map: map}}
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

  def handle_info({_mac_event, list} = e, state = %Env{subs: subs, events: events})
      when is_list(list) do
    if subs == [] do
      {:noreply, %Env{state | events: events ++ [e]}}
    else
      for sub <- subs do
        send(sub, e)
      end

      {:noreply, state}
    end
  end

  def handle_info(
        {:DOWN, _ref, :process, pid, _reason},
        state = %Env{subs: subs, windows: windows}
      ) do
    {:noreply, %Env{state | windows: windows -- [pid], subs: subs -- [pid]}}
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

  @doc """
    Wrapper around wx.subscribe()
  
    Will send to the calling process events in the form:
  
    * `{:print_file, [filename]}`
    * `{:open_file, [filename]}`
    * `{:open_url, [filename]}`
    * `{:new_file, []}`
  """
  def subscribe() do
    GenServer.call(__MODULE__, {:subscribe, self()})
  end

  defp init_sni() do
    # We're protecting the main application from any possible
    # side effects of the SNI startup using a monitored (not linked) process:
    {task, ref} = spawn_monitor(fn -> exit(do_init_sni()) end)

    receive do
      {:DOWN, ^ref, :process, ^task, {:ok, pid}} ->
        pid

      {:DOWN, ^ref, :process, ^task, :not_supported} ->
        nil

      {:DOWN, ^ref, :process, ^task, reason} ->
        Logger.error("ExSNI.start crashed: #{inspect(reason)}")
        nil
    end
  end

  defp do_init_sni() do
    cond do
      Desktop.OS.type() != Linux ->
        :not_supported

      System.get_env("USE_DBUS_MENU", nil) == "false" ->
        :not_supported

      true ->
        case ExSni.start_link() do
          {:ok, pid} ->
            if ExSni.is_supported?(pid) do
              Process.unlink(pid)
              {:ok, pid}
            else
              ExSni.close(pid)
              :not_supported
            end

          error ->
            error
        end
    end
  end
end
