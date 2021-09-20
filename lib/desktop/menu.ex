defmodule Desktop.Menu do
  require Logger
  alias Desktop.Menu
  alias Desktop.Menu.{Adapter, Parser, Proxy}

  defstruct [
    :__adapter__,
    :assigns,
    :dom,
    :mod,
    :proxy
  ]

  @type t() :: %__MODULE__{
          __adapter__: any(),
          assigns: %{},
          mod: atom(),
          dom: any(),
          proxy: nil | pid()
        }

  @callback mount(Menu.t()) :: {:ok, Menu.t()}
  @callback handle_event(String.t(), Menu.t()) :: {:noreply, Menu.t()}
  @callback handle_info(any(), Menu.t()) :: {:noreply, Menu.t()}
  @callback render(Keyword.t()) :: String.t()

  @doc false
  defmacro __using__(opts) do
    quote do
      import Phoenix.LiveView, except: [assign: 2]
      @behaviour Desktop.Menu
      import Desktop.Menu, only: [assign: 2, escape: 1]
      import Phoenix.HTML, only: [sigil_e: 2, sigil_E: 2]

      if not Keyword.get(unquote(opts), :skip_render, false) do
        require EEx
        @template Path.basename(__ENV__.file, ".ex") <> ".eex"
        EEx.function_from_file(:def, :render, "#{__DIR__}/#{@template}", [:assigns])
        defoverridable render: 1
      end
    end
  end

  def escape(string) do
    Parser.escape(string)
  end

  def assign(menu = %Menu{assigns: assigns}, keywords \\ []) do
    assigns = Map.merge(assigns, Map.new(keywords))
    %{menu | assigns: assigns}
  end

  def new(module, env, adapter \\ Adapter.Wx)

  def new(module, env, adapter) do
    {:ok, proxy} = Proxy.start_link(nil)
    adapter = adapter.new(env, proxy)

    menu = %Menu{
      __adapter__: adapter,
      mod: module,
      dom: [],
      assigns: %{},
      proxy: proxy
    }

    Proxy.update(proxy, menu)
  end

  def create(%{__adapter__: nil}) do
    raise "Menu has no adapter set"
  end

  def create(menu = %{__adapter__: adapter, dom: dom, proxy: proxy}, opts) do
    menu = %{menu | __adapter__: Adapter.create(adapter, dom, opts)}

    # We need to `mount/2` the menu, inside a server process
    # that will receive `handle_info/2` calls.
    # And that is because Phoenix.PubSub.subscribe/2
    # takes the `pid` it's called under and uses that
    # to send messages, such as `:changed`
    Proxy.update(proxy, menu)
    Proxy.mount(proxy)
  end

  def update_dom(%{__adapter__: nil}) do
    raise "Menu has no adapter"
  end

  def update_dom(
        menu = %{__adapter__: adapter, mod: mod, dom: dom, assigns: assigns, proxy: proxy}
      ) do
    new_dom =
      mod.render(assigns)
      |> Parser.parse()

    if new_dom != dom do
      adapter = Adapter.update_dom(adapter, new_dom)
      menu = %{menu | __adapter__: adapter, dom: new_dom}

      if self() != proxy do
        Proxy.update(proxy, menu)
      else
        menu
      end
    else
      menu
    end
  end

  def trigger_event(menu = %Menu{mod: mod}, event) do
    try do
      mod.handle_event(event, menu)
    rescue
      _ -> menu
    else
      {:update, menu} -> update_dom(menu)
      _ -> menu
    end
  end

  def popup_menu(%{__adapter__: nil}) do
    raise "Menu has no adapter"
  end

  def popup_menu(%{__adapter__: adapter, dom: dom}) do
    Adapter.popup_menu(adapter, dom)
  end

  def menubar(%{__adapter__: nil}) do
    raise "Menu has no adapter"
  end

  def menubar(%{__adapter__: adapter}) do
    Adapter.menubar(adapter)
  end
end
