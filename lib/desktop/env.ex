defmodule Desktop.Env do
  @moduledoc """
  Env holds any needed :wx / Desktop application state. Currently
  it keeps track of
  * The open Desktop.Window(s),
  * OS Application events (such as when a file is dragged on the application icon)
  * The :wx environment
  * The dbus connection (sni) on linux

  Also it has a global connect() method to allow binding of :wx event callbacks using
  this long lived process as reference.

  ## Subscriber events

  Processes that call `subscribe/0` receive `t:subscriber_event/0` messages.

  Path and URL payloads are **canonical UTF-8 binaries** (`String.t()`). Wx and
  other sources may pass charlists; `Desktop.Env` normalizes them at the boundary
  (both on `notify_subscribers/1` and on OS `handle_info` ingest) before buffering
  or delivering to subscribers.

  * `{:print_file, [path]}` / `{:open_file, [path]}` / `{:open_url, [url]}` —
    zero or more path/URL binaries
  * `{:new_file, []}`
  * `{:desktop, :window_activated, window_id}` — a `Desktop.Window` with registered
    `id` atom has become the active frame
  """
  alias Desktop.Env
  use GenServer
  require Logger

  @typedoc "UTF-8 path or URL string delivered to Env subscribers."
  @type path_or_url :: String.t()

  @typedoc "OS application open/print events (paths and URLs are binaries)."
  @type os_app_event ::
          {:print_file, [path_or_url()]}
          | {:open_file, [path_or_url()]}
          | {:open_url, [path_or_url()]}
          | {:new_file, []}

  @typedoc "Desktop lifecycle events."
  @type desktop_event :: {:desktop, :window_activated, atom()}

  @typedoc "Messages delivered to processes that called `subscribe/0`."
  @type subscriber_event :: os_app_event() | desktop_event()

  @path_tags [:open_url, :open_file, :print_file]

  defstruct [:wx_env, :wx, :map, :waiters, :windows, :sni, :events, :subs]

  @doc false
  @spec start_link() :: :ignore | {:error, any} | {:ok, pid}
  def start_link() do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
  end

  @doc false
  @impl true
  def init(_arg) do
    {wx, wx_env} = Desktop.Platform.System.init_env()
    Desktop.Platform.System.subscribe_events()

    {:ok,
     %Env{
       wx_env: wx_env,
       wx: wx,
       map: %{},
       waiters: %{},
       windows: [],
       sni: :not_initialized,
       events: [],
       subs: []
     }}
  end

  @doc """
  Returns the currently used webview backend
  """
  def backend() do
    get(:webview_backend, "nil")
  end

  @impl true
  def handle_call({:subscribe, pid}, _from, d = %Env{events: events, subs: subs}) do
    for e <- events do
      send(pid, e)
    end

    subs =
      if pid in subs do
        subs
      else
        Process.monitor(pid)
        [pid | subs]
      end

    {:reply, :ok, %Env{d | subs: subs, events: []}}
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
    ret = Desktop.Platform.System.connect_menu(object, command, callback, id)
    {:reply, ret, d}
  end

  @impl true
  def handle_cast({:register_window, window}, state = %Env{windows: windows}) do
    Process.monitor(window)
    {:noreply, %Env{state | windows: [window | windows]}}
  end

  def handle_cast({:notify_subscribers, message}, state = %Env{subs: subs}) do
    message = normalize_subscriber_event(message)

    for sub <- subs do
      send(sub, message)
    end

    {:noreply, state}
  end

  @impl true
  def handle_info({:reopen_app, []}, state = %Env{windows: windows}) do
    case windows do
      [window | _] ->
        Debouncer.immediate2({Desktop, :reopen}, fn -> Desktop.Window.show(window) end, 500)

      [] ->
        :nothing
    end

    {:noreply, state}
  end

  def handle_info(:reconnect, state = %Env{}) do
    if Desktop.OS.type() == IOS do
      for endpoint <- endpoints() do
        if Kernel.function_exported?(:ranch, :suspend_listener, 1) do
          apply(:ranch, :suspend_listener, [endpoint])
          apply(:ranch, :resume_listener, [endpoint])
        end
      end
    end

    {:noreply, state}
  end

  def handle_info({_mac_event, list} = e, state = %Env{subs: subs, events: events})
      when is_list(list) do
    e = normalize_subscriber_event(e)

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

  @doc false
  def endpoints() do
    case :ets.whereis(:ranch_server) do
      :undefined ->
        []

      tid ->
        :ets.tab2list(tid)
        |> Map.new()
        |> Map.keys()
        |> Enum.filter(fn
          {:addr, _endpoint} -> true
          _ -> false
        end)
        |> Enum.map(fn {:addr, endpoint} -> endpoint end)
    end
  end

  @doc """
  Returns the raw ExSni handle if used under linux to talk to DBus.
  """
  def sni() do
    GenServer.call(__MODULE__, :sni)
  end

  @doc """
  Returns the wx object. This is what has been created by the application using `:wx.new/1`. You
  typically need a reference to this to execute raw `:wx` commands.
  """
  def wx() do
    GenServer.call(__MODULE__, :wx)
  end

  @doc """
  Gets the Desktop process's current raw `:wx` environment.
  Can be sent to other processes to allow them use this process wx environment.

  ## Example

      iex> :wx.set_env(Desktop.Env.wx_env())
      iex> :wxWebView.isContextMenuEnabled(Desktop.Window.webview(pid))
      false
  """
  def wx_env() do
    GenServer.call(__MODULE__, :wx_env)
  end

  @doc """
  Shortcut for `:wx.set_env(Desktop.Env.wx_env())`
  """
  def wx_use_env() do
    env =
      case Process.whereis(__MODULE__) do
        nil -> nil
        _ -> wx_env()
      end

    if env != nil do
      Desktop.Platform.System.set_env(env)
    end

    :ok
  end

  @doc false
  def connect(object, command, callback, id \\ nil) do
    GenServer.call(__MODULE__, {:connect, object, command, callback, id})
  end

  @doc false
  def put(key, value) do
    GenServer.call(__MODULE__, {:put, key, value})
  end

  @doc false
  def get(key, default \\ nil) do
    GenServer.call(__MODULE__, {:get, key, default})
  end

  @doc false
  def pop(key, default \\ nil) do
    GenServer.call(__MODULE__, {:pop, key, default})
  end

  @doc false
  def await(key) do
    GenServer.call(__MODULE__, {:await, key})
  end

  @doc """
  Subscribe the calling process to OS and desktop lifecycle events.

  Delivers `t:subscriber_event/0` messages. Path and URL lists are always
  UTF-8 binaries (never charlists), including when the OS or bridge originally
  supplied charlists.

  * `{:print_file, [path_or_url]}`
  * `{:open_file, [path_or_url]}`
  * `{:open_url, [path_or_url]}`
  * `{:new_file, []}`
  * `{:desktop, :window_activated, window_id}` — `window_id` is the window `id` atom
  """
  def subscribe() do
    GenServer.call(__MODULE__, {:subscribe, self()})
  end

  @doc """
  Delivers a `t:subscriber_event/0` to all processes that called `subscribe/0`.

  Used by `Desktop.Window` for lifecycle events and by native bridges (e.g.
  EventBridge) for OS open-URL/file notifications. Callers may pass charlist
  paths/URLs; they are normalized to binaries before delivery.
  """
  def notify_subscribers(message) when is_tuple(message) do
    GenServer.cast(__MODULE__, {:notify_subscribers, message})
  end

  defp normalize_subscriber_event({tag, paths})
       when tag in @path_tags and is_list(paths) do
    {tag, normalize_paths(paths)}
  end

  defp normalize_subscriber_event(other), do: other

  # Bare wx charlist as the entire second element: {:open_url, ~c"https://..."}.
  defp normalize_paths([c | _] = charlist) when is_integer(c) do
    [List.to_string(charlist)]
  end

  defp normalize_paths(paths) when is_list(paths) do
    Enum.map(paths, &normalize_path/1)
  end

  defp normalize_path(bin) when is_binary(bin), do: bin
  defp normalize_path(list) when is_list(list), do: List.to_string(list)

  defp init_sni() do
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
