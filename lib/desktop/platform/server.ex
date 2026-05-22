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
      case module.handle_event(message, state) do
        {:noreply, new_state} -> {:noreply, %{s | state: new_state}}
        other -> other
      end
    else
      dispatch_info(message, s)
    end
  end

  defp dispatch_info(message, s = %__MODULE__{state: state, module: module}) do
    if function_exported?(module, :handle_info, 2) do
      case module.handle_info(message, state) do
        {:noreply, new_state} -> {:noreply, %{s | state: new_state}}
        other -> other
      end
    else
      {:noreply, s}
    end
  end

  @impl true
  def handle_cast(message, s = %__MODULE__{state: state, module: module}) do
    case module.handle_cast(message, state) do
      {:noreply, new_state} -> {:noreply, %{s | state: new_state}}
      other -> other
    end
  end

  @impl true
  def handle_call(message, from, s = %__MODULE__{state: state, module: module}) do
    case module.handle_call(message, from, state) do
      {:noreply, new_state} -> {:noreply, %{s | state: new_state}}
      {:reply, reply, new_state} -> {:reply, reply, %{s | state: new_state}}
      other -> other
    end
  end
end
