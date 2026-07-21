defmodule Desktop.Bridge.Mock do
  @moduledoc false

  def send(_pid, message) do
    handle_message(message, self())
  end

  def handle_message(<<ref::unsigned-size(64), json::binary>>, from) do
    response = handle_method(Desktop.Bridge.Codec.decode!(json))
    reply = Desktop.Bridge.Codec.encode!(response)
    Kernel.send(from, {:tcp, __MODULE__, <<ref::unsigned-size(64), reply::binary>>})
  end

  def handle_method([:wx, :getObjectType, [arg]]), do: Keyword.get(arg, :type)
  def handle_method([:wxLocale | _]), do: ~c"en"
  def handle_method([:wx_misc, :launchDefaultBrowser | _]), do: :ok
  def handle_method([:wx_misc, :getOsDescription | _]), do: ~c"Mock OS"

  def handle_method([type, :new | args]),
    do: [id: System.unique_integer([:positive]), type: type, args: args]

  def handle_method([:wxMenuBar, :getMenuCount | _]), do: 0

  def handle_method([_module, method | _]) do
    case Atom.to_string(method) do
      <<"is", _::binary>> -> true
      <<"set", _::binary>> -> true
      _other -> :ok
    end
  end
end
