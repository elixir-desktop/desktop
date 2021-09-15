defimpl Desktop.Menu.Adapter, for: Desktop.Menu.Adapter.Wx do
  def create(adapter, dom, opts) do
    @for.create(adapter, dom, opts)
  end

  def update_dom(adapter) do
    @for.update_dom(adapter, %{})
  end

  def update_dom(adapter, dom) do
    @for.update_dom(adapter, dom)
  end

  def popup_menu(adapter, dom) do
    @for.popup_menu(adapter, dom)
  end

  def menubar(%{menubar: bar}) do
    bar
  end
end

defimpl Desktop.Menu.Adapter, for: Desktop.Menu.Adapter.DBus do
  def create(adapter, dom, opts) do
    @for.create(adapter, dom, opts)
  end

  def update_dom(adapter) do
    @for.update_dom(adapter, %{})
  end

  def update_dom(adapter, dom) do
    @for.update_dom(adapter, dom)
  end

  def popup_menu(adapter, dom) do
    @for.popup_menu(adapter, dom)
  end

  def menubar(%{menubar: bar}) do
    bar
  end
end
