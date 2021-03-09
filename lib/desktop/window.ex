defmodule Desktop.Window do
  alias Desktop.{OS, Window, Wx, Menu, Fallback}
  require Logger
  use GenServer

  @enforce_keys [:frame]
  defstruct [
    :module,
    :taskbar,
    :frame,
    :notifications,
    :webview,
    :home_url,
    :last_url,
    :title,
    :rebuild,
    :rebuild_timer
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

  @impl true
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
    :wxFrame.setSizer(frame, :wxBoxSizer.new(Wx.wxHORIZONTAL()))

    # This one-line version will not show right on MacOS:
    # icon = :wxIcon.new(Path.join(:code.priv_dir(app), icon))

    # This 5-line version does show right though:
    image = :wxImage.new(Path.join(:code.priv_dir(app), icon))
    bitmap = :wxBitmap.new(image)
    icon = :wxIcon.new()
    :wxIcon.copyFromBitmap(icon, bitmap)
    :wxBitmap.destroy(bitmap)

    :wxTopLevelWindow.setIcon(frame, icon)

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

    taskbar =
      if icon_menu do
        {:ok, pid} = Menu.start_link(icon_menu, env, {:taskbar, icon})

        :wxTaskBarIcon.connect(Menu.taskbar(pid), :taskbar_left_down, skip: true)
        :wxTaskBarIcon.connect(Menu.taskbar(pid), :taskbar_right_down, skip: true)

        pid
      end

    timer =
      if OS.type() == Windows do
        {:ok, timer} = :timer.send_interval(500, :rebuild)
        timer
      end

    ui = %Window{
      frame: frame,
      webview: Fallback.webview_new(frame),
      notifications: %{},
      home_url: url,
      title: window_title,
      taskbar: taskbar,
      rebuild: 0,
      rebuild_timer: timer
    }

    if not hidden do
      show(self(), url)
    end

    {frame, ui}
  end

  def show(pid, url \\ nil) do
    GenServer.cast(pid, {:show, url})
  end

  def webview(pid) do
    GenServer.call(pid, :webview)
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
        wx(event: {:wxTaskBarIcon, :taskbar_left_down}),
        menu = %Window{taskbar: taskbar}
      ) do
    Menu.popup_menu(taskbar)
    {:noreply, menu}
  end

  def handle_event(
        wx(event: {:wxTaskBarIcon, :taskbar_right_down}),
        menu = %Window{taskbar: taskbar}
      ) do
    Menu.popup_menu(taskbar)
    {:noreply, menu}
  end

  def handle_event(
        wx(event: wxClose(type: :close_window)),
        ui = %Window{frame: frame, taskbar: taskbar}
      ) do
    if taskbar == nil do
      :wxFrame.hide(frame)
      {:stop, :normal, ui}
    else
      :wxFrame.hide(frame)
      {:noreply, ui}
    end
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

  @impl true
  def handle_info(:rebuild, ui = %Window{rebuild: rebuild, rebuild_timer: t, webview: webview}) do
    ui =
      if Fallback.webview_can_fix(webview) do
        case rebuild do
          0 ->
            %Window{ui | rebuild: 1}

          1 ->
            :timer.cancel(t)
            %Window{ui | rebuild: :done, webview: Fallback.webview_rebuild(ui)}

          :done ->
            ui
        end
      else
        if rebuild == :done do
          ui
        else
          %Window{ui | rebuild: 0}
        end
      end

    {:noreply, ui}
  end

  @impl true
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

  @impl true
  def handle_cast({:show, url}, ui = %Window{home_url: home}) do
    url = prepare_url(url)
    Logger.info("Showing #{url || prepare_url(home)}")
    Fallback.webview_show(ui, url, prepare_url(home))
    {:noreply, %Window{ui | last_url: url}}
  end

  @impl true
  def handle_call(:webview, _from, ui = %Window{webview: webview}) do
    {:reply, webview, ui}
  end

  defp prepare_url(url) do
    query = "k=" <> Desktop.Auth.login_key()

    case url do
      nil -> nil
      fun when is_function(fun) -> append_query(fun.(), query)
      string when is_binary(string) -> append_query(string, query)
    end
  end

  defp append_query(url, query) do
    case URI.parse(url) do
      url = %URI{query: nil} -> %URI{url | query: query}
      url = %URI{query: other} -> %URI{url | query: other <> "&" <> query}
    end
    |> URI.to_string()
  end
end
