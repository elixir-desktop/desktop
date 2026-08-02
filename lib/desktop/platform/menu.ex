defmodule Desktop.Platform.Menu do
  @moduledoc false

  alias Desktop.Menu.Adapter

  @doc """
  Selects the menu adapter module for the current platform capabilities.
  """
  @spec adapter(keyword()) :: module()
  def adapter(opts \\ []) do
    caps = Desktop.Platform.capabilities()

    cond do
      mod = Application.get_env(:desktop, :menu_adapter) ->
        mod

      Keyword.get(opts, :adapter) ->
        Keyword.get(opts, :adapter)

      Keyword.get(opts, :sni) != nil and Desktop.Platform.backend() == Desktop.Backend.Wx ->
        Adapter.DBus

      caps.menu == :native ->
        Adapter.Json

      caps.menu == :wx ->
        Adapter.Wx

      true ->
        Adapter.Browser
    end
  end

  @spec menubar_opts(keyword()) :: term()
  def menubar_opts(opts) do
    caps = Desktop.Platform.capabilities()

    cond do
      match?({:taskbar, _}, Keyword.get(opts, :wx)) ->
        Keyword.get(opts, :wx)

      caps.menu == :wx ->
        menubar_new()

      true ->
        nil
    end
  end

  defp menubar_new do
    Desktop.Platform.Window.new_menubar()
  end
end
