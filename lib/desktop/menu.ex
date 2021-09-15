defmodule Desktop.Menu do
  alias Desktop.Menu
  alias Desktop.Menu.{Adapter, Parser}

  defstruct [
    :__adapter__,
    :assigns,
    :dom,
    :mod
  ]

  @type t() :: %__MODULE__{
          __adapter__: any(),
          assigns: %{},
          mod: atom(),
          dom: any()
        }

  @callback mount(Menu.t()) :: {:ok, Menu.t()}
  @callback handle_event(String.t(), Menu.t()) :: {:noreply, Menu.t()}
  @callback handle_info(any(), Menu.t()) :: {:noreply, Menu.t()}
  @callback render(Keyword.t()) :: String.t()

  @doc false
  defmacro __using__(opts) do
    quote do
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

  def assign(%Menu{assigns: assigns} = menu, keywords \\ []) do
    assigns = Map.merge(assigns, Map.new(keywords))
    %{menu | assigns: assigns}
  end

  def new(module, env, adapter \\ :wx)

  def new(module, env, :wx) do
    new(module, env, Adapter.Wx)
  end

  def new(module, env, :dbus) do
    new(module, env, Adapter.DBus)
  end

  def new(module, env, adapter) do
    adapter = adapter.new(env, module)

    %Menu{
      __adapter__: adapter,
      mod: module,
      dom: [],
      assigns: %{}
    }
  end

  def create(%{__adapter__: nil}) do
    raise "Menu has no adapter set"
  end

  def create(%{__adapter__: adapter, mod: mod, dom: dom} = menu, opts) do
    menu = %{menu | __adapter__: Adapter.create(adapter, dom, opts)}
    {:ok, menu} = mod.mount(menu)
    update_dom(menu)
  end

  def update_dom(%{__adapter__: nil}) do
    raise "Menu has no adapter"
  end

  def update_dom(%{__adapter__: adapter, mod: mod, dom: dom, assigns: assigns} = menu) do
    new_dom =
      mod.render(assigns)
      |> Parser.parse()

    if new_dom != dom do
      adapter = Adapter.update_dom(adapter, new_dom)
      %{menu | __adapter__: adapter, dom: new_dom}
    else
      menu
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
