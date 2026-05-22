defmodule Desktop.Bridge.Transport do
  @moduledoc false
  use GenServer
  require Logger

  @name __MODULE__

  defstruct port: nil,
            socket: nil,
            send: nil,
            requests: %{},
            funs: %{},
            events: [],
            subscribers: [],
            last_url: nil

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: @name)
  end

  def ensure_started do
    case Process.whereis(@name) do
      nil ->
        {:ok, pid} = start_link([])
        pid

      pid ->
        pid
    end
  end

  def register_fun(fun) do
    GenServer.call(@name, {:register_fun, fun})
  end

  def subscribe_events(pid) do
    GenServer.call(@name, {:subscribe_events, pid})
  end

  def bridge_call(:wx, :batch, [fun]), do: fun.()
  def bridge_call(:wx, :set_env, _args), do: :ok
  def bridge_call(:wx, :get_env, _args), do: :ok
  def bridge_call(:wx, :getObjectType, [obj]), do: Keyword.get(obj, :type)

  def bridge_call(:wxWebView, :loadURL, [obj, uri]) do
    GenServer.cast(@name, {:last_url, uri})
    do_bridge_call(:wxWebView, :loadURL, [obj, uri])
  end

  def bridge_call(:wx, :new, _args), do: ensure_started()

  def bridge_call(type, :new, args) do
    [id: System.unique_integer([:positive]), type: type, args: args]
  end

  def bridge_call(_type, :getId, args), do: Keyword.get(args, :id)

  def bridge_call(module, :connect, args) do
    ref = System.unique_integer([:positive]) + 10
    json = Desktop.Bridge.Codec.encode!([module, :connect, args])

    GenServer.cast(@name, {:bridge_call, ref, json})
    :ok
  end

  def bridge_call(module, method, args) do
    do_bridge_call(module, method, args)
  end

  defp do_bridge_call(module, method, args) do
    ref = System.unique_integer([:positive]) + 10
    json = Desktop.Bridge.Codec.encode!([module, method, args])

    case GenServer.call(@name, {:bridge_call, ref, json}) do
      response when is_binary(response) -> Desktop.Bridge.Codec.decode!(response)
      other -> other
    end
  end

  @impl true
  def init(_opts) do
    port = String.to_integer(System.get_env("BRIDGE_PORT", "0"))

    {socket, send} =
      if port == 0 do
        {Desktop.Bridge.Mock, &Desktop.Bridge.Mock.send/2}
      else
        {:ok, socket} =
          :gen_tcp.connect(~c"127.0.0.1", port, packet: 4, active: true, mode: :binary)

        {socket, &:gen_tcp.send/2}
      end

    {:ok,
     %__MODULE__{
       port: port,
       socket: socket,
       send: send
     }}
  end

  @impl true
  def handle_cast({:bridge_call, ref, json}, state) do
    case handle_call({:bridge_call, ref, json}, nil, state) do
      {:reply, _ret, state} -> {:noreply, state}
      {:noreply, state} -> {:noreply, state}
    end
  end

  def handle_cast({:last_url, uri}, state = %__MODULE__{}) do
    {:noreply, %__MODULE__{state | last_url: uri}}
  end

  @impl true
  def handle_call(
        {:subscribe_events, pid},
        _from,
        state = %__MODULE__{events: events, subscribers: subs}
      ) do
    for event <- events do
      send(pid, event)
    end

    {:reply, :ok, %__MODULE__{state | events: [], subscribers: [pid | subs]}}
  end

  def handle_call(
        {:bridge_call, ref, json},
        from,
        state = %__MODULE__{socket: socket, requests: reqs, send: send}
      ) do
    if socket do
      message = <<ref::unsigned-size(64), json::binary>>
      send.(socket, message)
      {:noreply, %__MODULE__{state | requests: Map.put(reqs, ref, {from, message})}}
    else
      {:reply, ":ok", state}
    end
  end

  def handle_call({:register_fun, fun}, _from, state = %__MODULE__{funs: funs}) do
    ref = System.unique_integer([:positive])
    {:reply, ref, %__MODULE__{state | funs: Map.put(funs, ref, fun)}}
  end

  @impl true
  def handle_info(
        {:tcp, _port, <<0::unsigned-size(64), json::binary>>},
        state = %__MODULE__{subscribers: subs, events: events}
      ) do
    event = Desktop.Bridge.Codec.decode!(json)

    if subs == [] do
      {:noreply, %__MODULE__{state | events: events ++ [event]}}
    else
      for sub <- subs, do: send(sub, event)
      {:noreply, state}
    end
  end

  def handle_info(
        {:tcp, _port, <<1::unsigned-size(64), fun_ref::unsigned-size(64), json::binary>>},
        state = %__MODULE__{funs: funs}
      ) do
    args = Desktop.Bridge.Codec.decode!(json)

    case Map.get(funs, fun_ref) do
      nil -> :ok
      fun -> spawn(fn -> apply(fun, args) end)
    end

    {:noreply, state}
  end

  def handle_info({:tcp, _port, <<2::unsigned-size(64), json::binary>>}, state) do
    json = Desktop.Bridge.Codec.decode!(json)
    payload = json[:payload]
    pid = json[:pid]

    if is_pid(pid), do: send(pid, payload)

    {:noreply, state}
  end

  def handle_info(
        {:tcp, _port, <<ref::unsigned-size(64), json::binary>>},
        state = %__MODULE__{requests: reqs}
      ) do
    {from, message} = Map.fetch!(reqs, ref)

    if json == "use_mock" do
      Desktop.Bridge.Mock.send(Desktop.Bridge.Mock, message)
      {:noreply, state}
    else
      if from, do: GenServer.reply(from, json)
      {:noreply, %__MODULE__{state | requests: Map.delete(reqs, ref)}}
    end
  end

  def handle_info({:tcp_error, socket, reason}, state = %__MODULE__{socket: socket}) do
    Logger.error("Bridge connection failed: #{inspect(reason)}")
    {:noreply, try_reconnect(state)}
  end

  def handle_info({:tcp_closed, socket}, state = %__MODULE__{socket: socket}) do
    Logger.error("Bridge connection closed")
    {:noreply, try_reconnect(state)}
  end

  def handle_info(_other, state), do: {:noreply, state}

  defp try_reconnect(state = %__MODULE__{port: port, last_url: last_url}) do
    case :gen_tcp.connect(~c"127.0.0.1", port, [packet: 4, active: true, mode: :binary], 1_000) do
      {:ok, socket} ->
        if last_url, do: spawn(fn -> bridge_call(:wxWebView, :loadURL, [nil, last_url]) end)
        %__MODULE__{state | socket: socket}

      {:error, _} ->
        Process.sleep(1_000)
        try_reconnect(state)
    end
  end
end
