defmodule Desktop.Window do
  alias Desktop.{OS, Window, Wx, Menu, Fallback}
  require Logger
  use GenServer

  @enforce_keys [:frame]
  defstruct [
    :module,
    :bar,
    :frame,
    :notifications,
    :webview,
    :home_url,
    :title
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

    env = Desktop.Env.wx_env()
    :wx.set_env(env)

    frame =
      :wxFrame.new(Desktop.Env.wx(), Wx.wxID_ANY(), window_title, [
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
      # if OS.type() == MacOS do
      #   :wxMenuBar.oSXGetAppleMenu(:wxMenuBar.new())
      # else
      {:ok, pid} = Menu.start_link(menubar, env, :wxMenuBar.new())
      :wxFrame.setMenuBar(frame, Menu.menubar(pid))
    else
      # MacOS osMenu
      if OS.type() == MacOS do
        :wxMenu.connect(:wxMenuBar.oSXGetAppleMenu(:wxMenuBar.new()), :command_menu_selected)
      end
    end

    if icon_menu do
      bar =
        :wxTaskBarIcon.new(
          createPopupMenu: fn ->
            {time, value} =
              :timer.tc(fn ->
                Menu.create_menu(icon_menu)
              end)

            ms = div(time, 1000)

            if ms > 100 do
              Logger.warning("createPopup took #{ms}ms")
            end

            value
          end
        )

      {:ok, pid} = Menu.start_link(icon_menu, env, bar)
      true = :wxTaskBarIcon.setIcon(bar, icon)

      OnCrash.call(pid, fn ->
        :wx.set_env(env)
        :wxTaskBarIcon.removeIcon(bar)
        :wxTaskBarIcon.destroy(bar)
      end)
    end

    ui = %Window{
      frame: frame,
      webview: webview,
      notifications: %{},
      home_url: url,
      title: window_title
    }

    if not hidden do
      show(self(), url)
    end

    {frame, ui}
  end

  def show(pid, url \\ nil) do
    GenServer.cast(pid, {:show, url})
  end

  def show_notification(pid, text, opts \\ []) do
    id = Keyword.get(opts, :id, :default)

    type =
      case Keyword.get(opts, :type, :info) do
        :info -> :info
        :error -> :error
        :warn -> :warning
        :warning -> :warning
      end

    title = Keyword.get(opts, :title, nil)

    timeout =
      case Keyword.get(opts, :timeout, :auto) do
        :auto -> -1
        :never -> 0
        ms when is_integer(ms) -> ms
      end

    callback = Keyword.get(opts, :callback, nil)
    GenServer.cast(pid, {:show_notification, text, id, type, title, callback, timeout})
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

  def handle_event(wx(obj: obj, event: wxCommand(type: :notification_message_click)), ui) do
    notification(ui, obj, :click)
    {:noreply, ui}
  end

  def handle_event(wx(obj: obj, event: wxCommand(type: :notification_message_dismissed)), ui) do
    notification(ui, obj, :dismiss)
    {:noreply, ui}
  end

  def handle_event(
        wx(obj: obj, event: wxCommand(commandInt: action, type: :notification_message_action)),
        ui
      ) do
    notification(ui, obj, {:action, action})
    {:noreply, ui}
  end

  defp notification(%Window{notifications: noties}, obj, action) do
    case Enum.find(noties, fn {_, {wx_ref, _callback}} -> wx_ref == obj end) do
      nil ->
        Logger.error(
          "Received unhandled notification event #{inspect(obj)}: #{inspect(action)} (#{
            inspect(noties)
          })"
        )

      {_, {_ref, nil}} ->
        :ok

      {_, {_ref, callback}} ->
        spawn(fn -> callback.(action) end)
    end
  end

  def handle_cast(
        {:show_notification, message, id, type, title, callback, timeout},
        ui = %Window{notifications: noties, title: window_title}
      ) do
    {n, _} =
      note =
      case Map.get(noties, id, nil) do
        nil -> {Fallback.notification_new(title || window_title, type), callback}
        {note, _} -> {note, callback}
      end

    Fallback.notification_show(n, message, timeout)
    noties = Map.put(noties, id, note)
    {:noreply, %Window{ui | notifications: noties}}
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
