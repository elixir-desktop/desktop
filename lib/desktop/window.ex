defmodule Desktop.Window do
  alias Desktop.{OS, Window, Wx, Menu, Fallback}
  require Logger
  use GenServer

  @enforce_keys [:frame]
  defstruct [
    :module,
    :bar,
    :frame,
    :notification,
    :webview,
    :home_url
  ]

  # options: module, title
  def child_spec(opts) do
    app = Keyword.fetch!(opts, :app)
    id = Keyword.fetch!(opts, :id)

    %{
      id: id,
      start: {__MODULE__, :start_link, [opts ++ [app: app, id: id]]}
    }
  end

  def start_link(opts) do
    id = Keyword.fetch!(opts, :id)
    {_ref, _num, _type, pid} = :wx_object.start_link({:local, id}, __MODULE__, opts, [])
    {:ok, pid}
  end

  def init(options) do
    window_title = options[:title] || Atom.to_string(options[:id])
    size = options[:size] || {600, 500}
    app = options[:app]
    icon = options[:icon] || "icon.png"
    menubar = options[:menubar]
    icon_menu = options[:icon_menu]
    hidden = options[:hidden] || false
    url = options[:url]

    :wx.set_env(Desktop.wx_env())

    frame =
      :wxFrame.new(Desktop.wx(), Wx.wxID_ANY(), window_title, [
        {:size, size},
        {:style, Wx.wxDEFAULT_FRAME_STYLE()}
      ])

    :wxFrame.connect(frame, :close_window)

    # This one-line version will not show right on MacOS:
    # icon = :wxIcon.new(Path.join(:code.priv_dir(app), icon))

    # This 5-line version does show right though:
    image = :wxImage.new(Path.join(:code.priv_dir(app), icon))
    bitmap = :wxBitmap.new(image)
    icon = :wxIcon.new()
    :wxIcon.copyFromBitmap(icon, bitmap)
    :wxBitmap.destroy(bitmap)

    :wxTopLevelWindow.setIcon(frame, icon)
    webview = Fallback.webview_new(frame)

    if menubar do
      bar =
        if OS.type() == MacOS do
          :wxMenuBar.oSXGetAppleMenu(:wxMenuBar.new())
        else
          :wxMenuBar.new()
        end

      {:ok, _pid} = Menu.start_link(menubar, :wx.get_env(), bar)
      :wxFrame.setMenuBar(frame, Menu.create_menu_bar(menubar, bar))
    else
      # MacOS osMenu
      if OS.type() == MacOS do
        :wxMenu.connect(:wxMenuBar.oSXGetAppleMenu(:wxMenuBar.new()), :command_menu_selected)
      end
    end

    if icon_menu do
      bar = :wxTaskBarIcon.new(createPopupMenu: fn -> Menu.create_menu(icon_menu) end)
      {:ok, _pid} = Menu.start_link(icon_menu, :wx.get_env(), bar)
      true = :wxTaskBarIcon.setIcon(bar, icon)
    end

    notification = Fallback.notification_new(window_title, :info)

    ui = %Window{
      frame: frame,
      webview: webview,
      notification: notification,
      home_url: url
    }

    if not hidden do
      show(self(), url)
    end

    {frame, ui}
  end

  def show(pid, url \\ nil) do
    GenServer.cast(pid, {:show, url})
  end

  def show_notification(pid, text) do
    GenServer.cast(pid, {:show_notification, text})
  end

  def quit() do
    OS.shutdown()
  end

  require Record

  for tag <- [:wx, :wxCommand, :wxClose] do
    Record.defrecordp(tag, Record.extract(tag, from_lib: "wx/include/wx.hrl"))
  end

  def handle_event(
        wx(event: wxClose(type: :close_window)),
        ui = %Window{frame: frame}
      ) do
    :wxFrame.hide(frame)
    {:noreply, ui}
  end

  def handle_event(wx(event: {:wxWebView, :webview_newwindow, _, _, _target, url}), ui) do
    :wx_misc.launchDefaultBrowser(url)
    {:noreply, ui}
  end

  def handle_cast({:show_notification, message}, ui = %Window{notification: notification}) do
    Fallback.notification_show(notification, message)
    {:noreply, ui}
  end

  def handle_cast({:show, url}, ui = %Window{home_url: home}) do
    Logger.info("Showing #{prepare_url(url)}")
    Fallback.webview_show(ui, prepare_url(url), prepare_url(home))
    {:noreply, ui}
  end

  defp prepare_url(url) do
    key = "?k=" <> Desktop.Auth.login_key()

    case url do
      nil -> nil
      fun when is_function(fun) -> fun.() <> key
      string when is_binary(string) -> string <> key
    end
  end
end
