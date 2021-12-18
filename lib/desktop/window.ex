defmodule Desktop.Window do
  @moduledoc ~S"""
  Defines a Desktop Window.

  The window hosts a Phoenix Endpoint and displays its content.
  It should be part of a supervision tree and is the main interface
  to interact with your application.

  In total the window is doing:

    * Displaying the endpoint content

    * Hosting and starting an optional menu bar

    * Controlling a taskbar icon if present

  ## The Window

  You can add the Window to your own Supervision tree:

      children = [{
        Desktop.Window,
        [
          app: :your_app,
          id: YourAppWindow,
          title: "Your App Title",
          size: {600, 500},
          icon: "icon.png",
          menubar: YourApp.MenuBar,
          icon_menu: YourApp.Menu,
          url: fn -> YourAppWeb.Router.Helpers.live_url(YourAppWeb.Endpoint, YourAppWeb.YourAppLive) end
        ]
      }]


  ### Window configuration

  In order to change the appearance of the application window these options can be defined:

    * `:app` - your app name within which the Window is running.

    * `:id` - an atom identifying the window. Can later be used to control the
      window using the functions of this module.

    * `:title` - the window title that will be show initially. The window
      title can be set later using `set_title/2`.

    * `:size` - the initial windows size in pixels {width, height}.
  
    * `:min_size` - the minimum windows size in pixels {width, height}.
  
    * `:hidden` - whether the window should be initially hidden defaults to false,
                  but is ignored on mobile platforms

        Possible values are:

        * `false` - Show the window on startup (default)
        * `true` - Don't show the window on startup

    * `:icon` - an icon file name that will be used as taskbar and
      window icon. Supported formats are png files

    * `:menubar` - an optional MenuBar module that will be rendered
      as the windows menu bar when given.

    * `:icon_menu` - an optional MenuBar module that will be rendered
      as menu onclick on the taskbar icon.

    * `:url` - a callback to the initial (default) url to show in the
      window.

  """

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

  @doc false
  def child_spec(opts) do
    app = Keyword.fetch!(opts, :app)
    id = Keyword.fetch!(opts, :id)

    %{
      id: id,
      start: {__MODULE__, :start_link, [opts ++ [app: app, id: id]]}
    }
  end

  @doc false
  def start_link(opts) do
    id = Keyword.fetch!(opts, :id)
    {_ref, _num, _type, pid} = :wx_object.start_link({:local, id}, __MODULE__, opts, [])
    {:ok, pid}
  end

  @impl true
  @doc false
  def init(options) do
    window_title = options[:title] || Atom.to_string(options[:id])
    size = options[:size] || {600, 500}
    min_size = options[:min_size]
    app = options[:app]
    icon = options[:icon]
    # not supported on mobile atm
    menubar = unless OS.mobile?(), do: options[:menubar]
    icon_menu = unless OS.mobile?(), do: options[:icon_menu]
    hidden = unless OS.mobile?(), do: options[:hidden]
    url = options[:url]

    env = Desktop.Env.wx_env()
    GenServer.cast(Desktop.Env, {:register_window, self()})
    :wx.set_env(env)

    frame =
      :wxFrame.new(Desktop.Env.wx(), Wx.wxID_ANY(), window_title, [
        {:size, size},
        {:style, Wx.wxDEFAULT_FRAME_STYLE()}
      ])

    :wxFrame.connect(frame, :close_window,
      callback: &close_window/2,
      userData: self()
    )

    if min_size do
      :wxFrame.setMinSize(frame, min_size)
    end

    :wxFrame.setSizer(frame, :wxBoxSizer.new(Wx.wxHORIZONTAL()))

    {:ok, icon} =
      case icon do
        nil -> {:ok, :wxArtProvider.getIcon("wxART_EXECUTABLE_FILE")}
        filename -> Desktop.Image.new_icon(app, filename)
      end

    :wxTopLevelWindow.setIcon(frame, icon)

    wx_menubar =
      if menubar do
        {:ok, menu_pid} =
          Menu.start_link(
            module: menubar,
            app: app,
            env: env,
            wx: :wxMenuBar.new()
          )

        wx_menubar = Menu.menubar(menu_pid)
        :wxFrame.setMenuBar(frame, wx_menubar)
        wx_menubar
      end

    if OS.type() == MacOS do
      update_apple_menu(window_title, frame, wx_menubar || :wxMenuBar.new())
    end

    taskbar =
      if icon_menu do
        sni_link = Desktop.Env.sni()
        adapter = if sni_link == nil, do: nil, else: Menu.Adapter.DBus

        {:ok, menu_pid} =
          Menu.start_link(
            module: icon_menu,
            app: app,
            adapter: adapter,
            env: env,
            sni: sni_link,
            icon: icon,
            wx: {:taskbar, icon}
          )

        menu_pid
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

    if hidden != true do
      show(self(), url)
    end

    {frame, ui}
  end

  @doc """
  Returns the url currently shown of the Window.

    * `pid` - The pid or atom of the Window

  ## Examples

      iex> Desktop.Window.url(pid)
      http://localhost:1234/main

  """
  def url(pid) do
    GenServer.call(pid, :url)
  end

  @doc """
  Show the Window if not visible with the given url.

    * `pid` - The pid or atom of the Window
    * `url` - The endpoint url to show. If non is provided
      the url callback will be used to get one.

  ## Examples

      iex> Desktop.Window.show(pid, "/")
      :ok

  """
  def show(pid, url \\ nil) do
    GenServer.cast(pid, {:show, url})
  end

  @doc """
  Hide the Window if visible (noop on mobile platforms)

    * `pid` - The pid or atom of the Window

  ## Examples

      iex> Desktop.Window.hide(pid)
      :ok

  """
  def hide(pid) do
    GenServer.cast(pid, :hide)
  end

  @doc """
  Returns true if the window is hidden. Always returns false
  on mobile platforms.

    * `pid` - The pid or atom of the Window

  ## Examples

      iex> Desktop.Window.is_hidden?(pid)
      false

  """
  def is_hidden?(pid) do
    GenServer.call(pid, :is_hidden?)
  end

  @doc """
  Set the windows title

    * `pid` - The pid or atom of the Window
    * `title` - The new windows title

  ## Examples

      iex> Desktop.Window.set_title(pid, "New Window Title")
      :ok

  """
  def set_title(pid, title) do
    GenServer.cast(pid, {:set_title, title})
  end

  @doc """
  Iconize or restore the window

    * `pid` - The pid or atom of the Window
    * `restore` - Optional defaults to false whether the
                  window should be restored
  """
  def iconize(pid, iconize \\ true) do
    GenServer.cast(pid, {:iconize, iconize})
  end

  @doc """
  Rebuild the webview. This function is a troubleshooting
  function at this time. On Windows it's sometimes necessary
  to rebuild the WebView2 frame.

    * `pid` - The pid or atom of the Window

  ## Examples

      iex> Desktop.Window.rebuild_webview(pid)
      :ok

  """
  def rebuild_webview(pid) do
    GenServer.cast(pid, :rebuild_webview)
  end

  @doc """
  Fetch the underlying :wxWebView instance object. Call
  this if you have to use more advanced :wxWebView functions
  directly on the object.

    * `pid` - The pid or atom of the Window

  ## Examples

      iex> :wx.set_env(Desktop.Env.wx_env())
      iex> :wxWebView.isContextMenuEnabled(Desktop.Window.webview(pid))
      false

  """
  def webview(pid) do
    GenServer.call(pid, :webview)
  end

  @doc """
  Fetch the underlying :wxFrame instance object. This represents
  the window which the webview is drawn into.

    * `pid` - The pid or atom of the Window

  ## Examples

      iex> :wx.set_env(Desktop.Env.wx_env())
      iex> :wxWindow.show(Desktop.Window.frame(pid), show: false)
      false

  """
  def frame(pid) do
    GenServer.call(pid, :frame)
  end

  @doc """
  Show a desktop notification

    * `pid` - The pid or atom of the Window

    * `text` - The text content to show in the notification

    * `opts` - Additional notification options

      Valid keys are:

        * `:id` - An id for the notification, this is important if you
          want control, the visibility of the notification. The default
          value when none is provided is `:default`

        * `:type` - One of `:info` `:error` `:warn` these will change
          how the notification will be displayed. The default is `:info`

        * `:title` - An alternative title for the notificaion,
          when none is provided the current window title is used.

        * `:timeout` - A timeout hint specifying how long the notification
          should be displayed.

          Possible values are:

            * `:auto` - This is the default and let's the OS decide

            * `:never` - Indicates that notification should not be hidden
              automatically

            * ms - A time value in milliseconds, how long the notification
              should be shown

        * `:callback` - A function to be executed when the user clicks on the
          notification.

  ## Examples

      iex> :wx.set_env(Desktop.Env.wx_env())
      iex> :wxWebView.isContextMenuEnabled(Desktop.Window.webview(pid))
      false

  """
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

  @doc """
  Quit the application. This forces a quick termination which can
  be helpful on MacOS/Windows as sometimes the destruction is
  crashing.
  """
  def quit() do
    OS.shutdown()
  end

  require Record

  for tag <- [:wx, :wxCommand, :wxClose] do
    Record.defrecordp(tag, Record.extract(tag, from_lib: "wx/include/wx.hrl"))
  end

  @doc false
  def handle_event(wx(event: {:wxWebView, :webview_newwindow, _, _, _target, url}), ui) do
    :wx_misc.launchDefaultBrowser(url)
    {:noreply, ui}
  end

  def handle_event(wx(id: id, event: wxCommand(type: :command_menu_selected)), ui) do
    if id == Wx.wxID_EXIT() do
      quit()
    end

    {:noreply, ui}
  end

  def handle_event(wx(obj: obj, event: wxCommand(type: :notification_message_click)), ui) do
    notification(ui, obj, :click)
    {:noreply, ui}
  end

  def handle_event(wx(obj: obj, event: wxCommand(type: :notification_message_dismissed)), ui) do
    notification(ui, obj, :dismiss)

    if OS.type() == Linux do
      notification(ui, obj, :action)
    else
      notification(ui, obj, :dismiss)
    end

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
  @doc false
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

  def close_window(wx(userData: pid), inev) do
    # if we don't veto vetoable events on MacOS the app freezes.
    if :wxCloseEvent.canVeto(inev) do
      :wxCloseEvent.veto(inev)
    end

    GenServer.cast(pid, :close_window)
    :ok
  end

  @impl true
  @doc false
  def handle_cast(:close_window, ui = %Window{frame: frame, taskbar: taskbar}) do
    if not :wxFrame.isShown(frame) do
      OS.shutdown()
    end

    if taskbar == nil do
      :wxFrame.hide(frame)
      {:stop, :normal, ui}
    else
      :wxFrame.hide(frame)
      {:noreply, ui}
    end
  end

  def handle_cast({:set_title, title}, ui = %Window{title: old, frame: frame}) do
    if title != old and frame != nil do
      :wxFrame.setTitle(frame, String.to_charlist(title))
    end

    {:noreply, %Window{ui | title: title}}
  end

  def handle_cast({:iconize, iconize}, ui = %Window{frame: frame}) do
    :wxTopLevelWindow.iconize(frame, iconize: iconize)
    {:noreply, ui}
  end

  def handle_cast(:rebuild_webview, ui) do
    {:noreply, %Window{ui | webview: Fallback.webview_rebuild(ui)}}
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

    Fallback.notification_show(n, message, timeout, title || window_title)
    noties = Map.put(noties, id, note)
    {:noreply, %Window{ui | notifications: noties}}
  end

  def handle_cast({:show, url}, ui = %Window{home_url: home, last_url: last}) do
    new_url = prepare_url(url || last || home)
    Logger.info("Showing #{new_url}")
    Fallback.webview_show(ui, new_url, url == nil)
    {:noreply, %Window{ui | last_url: new_url}}
  end

  @impl true
  def handle_cast(:hide, ui = %Window{frame: frame}) do
    if frame do
      :wxWindow.hide(frame)
    end

    {:noreply, ui}
  end

  @impl true
  @doc false
  def handle_call(:is_hidden?, _from, ui = %Window{frame: frame}) do
    ret =
      if frame do
        not :wxWindow.isShown(frame)
      else
        false
      end

    {:reply, ret, ui}
  end

  def handle_call(:url, _from, ui) do
    ret =
      case Fallback.webview_url(ui) do
        url when is_list(url) -> List.to_string(url)
        other -> other
      end

    {:reply, ret, ui}
  end

  def handle_call(:webview, _from, ui = %Window{webview: webview}) do
    {:reply, webview, ui}
  end

  def handle_call(:frame, _from, ui = %Window{frame: frame}) do
    {:reply, frame, ui}
  end

  def prepare_url(url) do
    query = "k=" <> Desktop.Auth.login_key()

    case url do
      nil -> nil
      fun when is_function(fun) -> append_query(fun.(), query)
      string when is_binary(string) -> append_query(string, query)
    end
  end

  defp append_query(url, query) do
    case URI.parse(url) do
      url = %URI{query: nil} ->
        %URI{url | query: query}

      url = %URI{query: other} ->
        if String.contains?(other, query) do
          url
        else
          %URI{url | query: other <> "&" <> query}
        end
    end
    |> URI.to_string()
  end

  defp update_apple_menu(title, frame, menubar) do
    menu = :wxMenuBar.oSXGetAppleMenu(menubar)
    :wxMenu.setTitle(menu, title)

    # Remove all items except for Quit since we don't yet handle the standard items
    # like "Hide <app>", "Hide Others", "Show All", etc
    for item <- :wxMenu.getMenuItems(menu) do
      if :wxMenuItem.getId(item) == Wx.wxID_EXIT() do
        :wxMenuItem.setText(item, "Quit #{title}\tCtrl+Q")
      else
        :wxMenu.delete(menu, item)
      end
    end

    :wxFrame.connect(frame, :command_menu_selected)
  end
end
