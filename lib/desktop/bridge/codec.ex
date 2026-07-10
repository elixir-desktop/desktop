defmodule Desktop.Bridge.Codec do
  @moduledoc false

  def encode!(var) do
    pre_encode!(var)
    |> Jason.encode!()
  end

  def decode!(json) do
    Jason.decode!(json) |> decode()
  end

  def pre_encode!(var) do
    case var do
      tuple when is_tuple(tuple) ->
        pre_encode!(%{_type: :tuple, value: Tuple.to_list(tuple)})

      pid when is_pid(pid) ->
        pre_encode!(%{_type: :pid, value: List.to_string(:erlang.pid_to_list(pid))})

      fun when is_function(fun) ->
        pre_encode!(%{_type: :fun, value: Desktop.Bridge.Transport.register_fun(fun)})

      list when is_list(list) ->
        Enum.map(list, &pre_encode!/1)

      map when is_map(map) ->
        Enum.reduce(map, %{}, fn {key, value}, map ->
          Map.put(map, pre_encode!(key), pre_encode!(value))
        end)

      atom when is_atom(atom) ->
        ":" <> Atom.to_string(atom)

      other ->
        other
    end
  end

  defp decode(list) when is_list(list), do: Enum.map(list, &decode/1)
  defp decode(":" <> name), do: String.to_atom(name)

  defp decode(map) when is_map(map) do
    Enum.reduce(map, %{}, fn {key, value}, ret ->
      Map.put(ret, decode(key), decode(value))
    end)
    |> decode_map()
  end

  defp decode(other), do: other

  defp decode_map(%{_type: :tuple, value: tuple}), do: List.to_tuple(tuple)

  defp decode_map(%{_type: :pid, value: pid}) do
    :erlang.list_to_pid(String.to_charlist(pid))
  end

  defp decode_map(other), do: other
end
