defprotocol Desktop.Menu.Adapter do
  @fallback_to_any true

  @spec create(t(), dom :: any()) :: t()
  def create(adapter, dom)

  @spec update_dom(t(), dom :: any()) :: t()
  def update_dom(adapter, dom)

  @spec popup_menu(t()) :: t()
  def popup_menu(adapter)

  @spec recreate_menu(t(), dom :: any()) :: t()
  def recreate_menu(adapter, dom)

  @spec menubar(t()) :: any()
  def menubar(adapter)

  @spec get_icon(t()) :: any()
  def get_icon(adapter)

  @spec set_icon(t(), any()) :: {:ok, any()} | {:error, any()}
  def set_icon(adapter, icon)
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

  def popup_menu(adapter = %{__struct__: module}) do
    module.popup_menu(adapter)
  end

  def recreate_menu(adapter = %{__struct__: module}, dom) do
    module.recreate_menu(adapter, dom)
  end

  def get_icon(adapter = %{__struct__: module}) do
    module.get_icon(adapter)
  end

  def set_icon(adapter = %{__struct__: module}, icon) do
    module.set_icon(adapter, icon)
  end

  def menubar(adapter = %{__struct__: module}) do
    module.menubar(adapter)
  end
end
