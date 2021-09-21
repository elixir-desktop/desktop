defprotocol Desktop.Menu.Adapter do
  @fallback_to_any true

  @spec create(t(), dom :: any()) :: t()
  def create(adapter, dom)

  @spec update_dom(t(), dom :: any()) :: t()
  def update_dom(adapter, dom)

  @spec popup_menu(t(), dom :: any()) :: t()
  def popup_menu(adapter, dom)

  @spec menubar(t()) :: any()
  def menubar(adapter)
end

defimpl Desktop.Menu.Adapter, for: Any do
  def create(adapter = %{__struct__: module}, dom) do
    module.create(adapter, dom)
  end

  def update_dom(adapter = %{__struct__: module}) do
    module.update_dom(adapter, %{})
  end

  def update_dom(adapter = %{__struct__: module}, dom) do
    module.update_dom(adapter, dom)
  end

  def popup_menu(adapter = %{__struct__: module}, dom) do
    module.popup_menu(adapter, dom)
  end

  def menubar(%{menubar: bar}) do
    bar
  end
end
