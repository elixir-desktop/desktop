defmodule Desktop.Platform.Server do
  @moduledoc false
  use GenServer

  defstruct frame: nil, state: nil, module: nil

  def start_link(name, module, args, _flags \\ []) do
    name =
      case name do
        {:local, name} -> name
        name -> name
      end

    {:ok, pid} = GenServer.start_link(__MODULE__, {module, args}, name: name)
    {:ref, 0, __MODULE__, pid}
  end

  @impl true
  def init({module, args}) do
    {frame, state} = module.init(args)
    {:ok, %__MODULE__{frame: frame, state: state, module: module}}
  end

  @impl true
  def handle_info(message, s = %__MODULE__{state: state, module: module}) do
    if function_exported?(module, :handle_event, 2) and is_tuple(message) do
      module.handle_event(message, state)
      |> wrap_result(s)
    else
      dispatch_info(message, s)
    end
  end

  defp dispatch_info(message, s = %__MODULE__{state: state, module: module}) do
    if function_exported?(module, :handle_info, 2) do
      module.handle_info(message, state)
      |> wrap_result(s)
    else
      {:noreply, s}
    end
  end

  @impl true
  def handle_cast(message, s = %__MODULE__{state: state, module: module}) do
    module.handle_cast(message, state)
    |> wrap_result(s)
  end

  @impl true
  def handle_call(message, from, s = %__MODULE__{state: state, module: module}) do
    module.handle_call(message, from, state)
    |> wrap_result(s)
  end

  defp wrap_result(result, s) do
    case result do
      {:reply, reply, new_state} ->
        {:reply, reply, %{s | state: new_state}}

      {:reply, reply, new_state, extra} ->
        {:reply, reply, %{s | state: new_state}, extra}

      {:noreply, new_state} ->
        {:noreply, %{s | state: new_state}}

      {:noreply, new_state, extra} ->
        {:noreply, %{s | state: new_state}, extra}

      {:stop, reason, new_state} ->
        {:stop, reason, %{s | state: new_state}}

      {:stop, reason, reply, new_state} ->
        {:stop, reason, reply, %{s | state: new_state}}

      other ->
        other
    end
  end
end
