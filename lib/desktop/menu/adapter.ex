defprotocol Desktop.Menu.Adapter do
  @spec create(t(), dom :: any(), opts :: any()) :: t()
  def create(adapter, dom, opts)

  @spec update_dom(t(), dom :: any()) :: t()
  def update_dom(adapter, dom)

  @spec popup_menu(t(), dom :: any()) :: t()
  def popup_menu(adapter, dom)

  @spec menubar(t()) :: any()
  def menubar(adapter)
end
