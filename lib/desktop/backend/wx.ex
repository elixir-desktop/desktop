defmodule Desktop.Backend.Wx do
  @moduledoc false

  alias Desktop.{Wx, OS}
  alias Desktop.Backend.Null

  @behaviour Desktop.Platform.Backend
  @behaviour Desktop.Platform.Window
  @behaviour Desktop.Platform.Content
  @behaviour Desktop.Platform.Notification
  @behaviour Desktop.Platform.Media
  @behaviour Desktop.Platform.System

  @impl true
  def capabilities do
    %{
      window: true,
      content: :webview,
      notification: :wx,
      menu: :wx,
      taskbar: true
    }
  end

  # System

  @impl true
  def init_env do
    wx = Null.wx_call(:wx, :new, [[]])
    Null.wx_call(:wx, :subscribe_events)
    {wx, Null.wx_call(:wx, :get_env)}
  end

  @impl true
  def subscribe_events, do: Null.wx_call(:wx, :subscribe_events) || :ok

  @impl true
  def set_env(env), do: Null.wx_call(:wx, :set_env, [env]) || :ok

  @impl true
  def get_env, do: Null.wx_call(:wx, :get_env)

  @impl true
  def locale do
    lang = Null.wx_call(:wxLocale, :getSystemLanguage, [])
    locale = Null.wx_call(:wxLocale, :new, [lang])
    Null.wx_call(:wxLocale, :getCanonicalName, [locale]) |> List.to_string() |> String.downcase()
  end

  @impl true
  def connect_menu(object, command, callback, id) do
    opts = [{:callback, fn _, _ -> callback.() end}]
    opts = if id == nil, do: opts, else: [{:id, id} | opts]
    Null.wx_call(:wxMenu, :connect, [object, command, opts])
  end

  @impl true
  def wx_available?, do: Null.wx_enabled?() and Null.module?(:wx)

  @impl true
  def open_external_url(url) do
    case OS.type() do
      MacOS ->
        Desktop.Impl.HostBrowser.open(url)

      Linux ->
        Desktop.Impl.HostBrowser.open(url)

      Windows ->
        Desktop.Impl.HostBrowser.open(url)

      _ ->
        Null.wx_call(:wx_misc, :launchDefaultBrowser, [String.to_charlist(url)])
        :ok
    end
  end

  @impl true
  def os_description do
    Null.wx_call(:wx_misc, :getOsDescription, [])
  end

  @impl true
  def custom_event(_event, _args), do: :ok

  @impl true
  def activate_event_active?(event) do
    if function_exported?(:wxActivateEvent, :getActive, 1) do
      :wxActivateEvent.getActive(event)
    else
      true
    end
  end

  # Window

  @impl true
  def open(opts) do
    wx = Keyword.fetch!(opts, :wx)
    title = Keyword.fetch!(opts, :title)
    size = Keyword.get(opts, :size, {600, 500})
    min_size = Keyword.get(opts, :min_size)
    icon = Keyword.get(opts, :icon)

    frame =
      Null.wx_call(:wxFrame, :new, [
        wx,
        Wx.wxID_ANY(),
        title,
        [{:size, size}, {:style, Wx.wxDEFAULT_FRAME_STYLE()}]
      ])

    if min_size, do: Null.wx_call(:wxFrame, :setMinSize, [frame, min_size])

    Null.wx_call(:wxFrame, :setSizer, [
      frame,
      Null.wx_call(:wxBoxSizer, :new, [Wx.wxHORIZONTAL()])
    ])

    {:ok, icon} =
      case icon do
        nil -> default_icon()
        other -> {:ok, other}
      end

    Null.wx_call(:wxTopLevelWindow, :setIcon, [frame, icon])
    webview = Desktop.Platform.Content.attach(frame)
    {:ok, frame, webview}
  end

  @impl true
  def destroy_frame(frame), do: Null.wx_call(:wxFrame, :destroy, [frame]) || :ok

  @impl true
  def connect(frame, event, fun) do
    opts = [callback: fun, userData: self()]
    Null.wx_call(:wxFrame, :connect, [frame, event, opts])
    :ok
  end

  @impl true
  def show(frame, opts) do
    show = Keyword.get(opts, :show, true)
    Null.wx_call(:wxFrame, :show, [frame, [show: show]])
    :ok
  end

  @impl true
  def hide(frame) do
    Null.wx_call(:wxFrame, :hide, [frame])
    :ok
  end

  @impl true
  def set_title(frame, title) do
    Null.wx_call(:wxFrame, :setTitle, [frame, String.to_charlist(title)])
    :ok
  end

  @impl true
  def set_min_size(frame, size), do: Null.wx_call(:wxFrame, :setMinSize, [frame, size]) || :ok

  @impl true
  def set_icon(frame, icon) do
    Null.wx_call(:wxTopLevelWindow, :setIcon, [frame, icon])
    :ok
  end

  @impl true
  def set_menubar(frame, menubar) do
    Null.wx_call(:wxFrame, :setMenuBar, [frame, menubar])
    :ok
  end

  @impl true
  def iconize(frame, iconize) do
    Null.wx_call(:wxTopLevelWindow, :iconize, [frame, [iconize: iconize]])
    :ok
  end

  @impl true
  def shown?(frame), do: Null.wx_call(:wxWindow, :isShown, [frame]) || false

  @impl true
  def active?(frame), do: Null.wx_call(:wxTopLevelWindow, :isActive, [frame]) || false

  @impl true
  def raise_window(nil), do: :ok

  def raise_window(frame) do
    case OS.type() do
      MacOS ->
        name = System.get_env("EMU", "beam.smp")

        spawn(fn ->
          try do
            System.cmd("open", ["-a", name], stderr_to_stdout: true, parallelism: true)
          rescue
            _ -> :ok
          end
        end)

      _ ->
        # Calling setFocus on wxDirDialog segfaults on macOS — handled above.
        Null.wx_call(:wxTopLevelWindow, :setFocus, [frame])
        Null.wx_call(:wxWindow, :raise, [frame])
    end

    :ok
  end

  @impl true
  def update_apple_menu(title, frame, menubar) do
    menu = Null.wx_call(:wxMenuBar, :oSXGetAppleMenu, [menubar])
    Null.wx_call(:wxMenu, :setTitle, [menu, title])

    for item <- Null.wx_call(:wxMenu, :getMenuItems, [menu]) || [] do
      if Null.wx_call(:wxMenuItem, :getId, [item]) == Wx.wxID_EXIT() do
        Null.wx_call(:wxMenuItem, :setText, [item, "Quit #{title}\tCtrl+Q"])
      else
        Null.wx_call(:wxMenu, :delete, [menu, item])
      end
    end

    Null.wx_call(:wxFrame, :connect, [frame, :command_menu_selected])
    :ok
  end

  @impl true
  def new_menubar, do: Null.wx_call(:wxMenuBar, :new, [])

  @impl true
  def on_crash_destroy(frame), do: destroy_frame(frame)

  @impl true
  def close_event_veto(inev) do
    if Null.wx_call(:wxCloseEvent, :canVeto, [inev]) do
      Null.wx_call(:wxCloseEvent, :veto, [inev])
    end

    :ok
  end

  # Content

  @impl true
  def attach(frame) do
    with :ok <- check_has_webview(),
         sizer <- clear_windows(frame),
         {:ok, webview} <- do_webview_new(frame) do
      Null.wx_call(:wxWebView, :connect, [webview, :webview_newwindow])
      Null.wx_call(:wxWebView, :connect, [webview, :webview_error])
      Null.wx_call(:wxWebView, :enableContextMenu, [webview, [enable: false]])

      Null.wx_call(:wxBoxSizer, :add, [sizer, webview, [proportion: 1, flag: Wx.wxEXPAND()]])
      Null.wx_call(:wxSizer, :layout, [sizer])
      Null.wx_call(:wxSizer, :show, [sizer, true])
      Null.wx_call(:wxFrame, :refresh, [frame])
      webview
    else
      {:error, _} -> nil
    end
  end

  @impl true
  def load_url(nil, _frame, url), do: open_external_url(url)

  def load_url(webview, _frame, url) do
    Null.wx_call(:wxWebView, :loadURL, [webview, url])
    :ok
  end

  @impl true
  def reload(nil), do: :ok

  def reload(webview) do
    Null.wx_call(:wxWebView, :reload, [webview])
    :ok
  end

  @impl true
  def current_url(nil, last_url), do: last_url

  def current_url(webview, _last_url) do
    case Null.wx_call(:wxWebView, :getCurrentURL, [webview]) do
      url when is_list(url) -> List.to_string(url)
      other -> other
    end
  end

  @impl true
  def content_show(nil, _frame, url, _), do: open_external_url(url)

  def content_show(webview, frame, url, _only_open) do
    if url, do: Null.wx_call(:wxWebView, :loadURL, [webview, url])

    if Null.wx_call(:wxTopLevelWindow, :isIconized, [frame]) do
      Null.wx_call(:wxTopLevelWindow, :iconize, [frame, [iconize: false]])
    end

    if not Null.wx_call(:wxWindow, :isShown, [frame]) do
      Null.wx_call(:wxWindow, :show, [frame, [show: true]])
      Null.wx_call(:wxTopLevelWindow, :centerOnScreen, [frame])
    end

    raise_window(frame)
  end

  @impl true
  def rebuild(nil, _url), do: nil

  def rebuild(frame, url) do
    webview = attach(frame)
    if url, do: Null.wx_call(:wxWebView, :loadURL, [webview, url])
    webview
  end

  @impl true
  def put_webview_backend(name) do
    Desktop.Env.put(:webview_backend, name)
    :ok
  end

  # Notification

  @impl true
  def new(title, type) do
    if Null.module?(:wxNotificationMessage) do
      flag =
        case type do
          :info -> Wx.wxICON_INFORMATION()
          :warning -> Wx.wxICON_WARNING()
          :error -> Wx.wxICON_ERROR()
        end

      notification = Null.wx_call(:wxNotificationMessage, :new, [title, [flags: flag]])

      if notification_events_available?() do
        for event <- [
              :notification_message_click,
              :notification_message_dismissed,
              :notification_message_action
            ] do
          Null.wx_call(:wxNotificationMessage, :connect, [notification, event])
        end
      end

      notification
    end
  end

  @impl true
  def notification_show(nil, message, _timeout, title) do
    require Logger
    Logger.notice("NOTIFICATION: #{title}: #{message}")
    :ok
  end

  def notification_show(notification, message, timeout, title) do
    if title,
      do: Null.wx_call(:wxNotificationMessage, :setTitle, [notification, to_charlist(title)])

    Null.wx_call(:wxNotificationMessage, :setMessage, [notification, to_charlist(message)])
    Null.wx_call(:wxNotificationMessage, :show, [notification, [timeout: timeout]])
    :ok
  end

  @impl true
  def close(notification) do
    Null.wx_call(:wxNotificationMessage, :close, [notification])
    :ok
  end

  # Media

  @impl true
  def load_image(app, path) do
    image = Null.wx_call(:wxImage, :new, [get_abs_path(app, path)])

    image =
      if Null.wx_call(:wxImage, :isOk, [image]) do
        image
      else
        fallback = Null.wx_call(:wxArtProvider, :getBitmap, ["wxART_ERROR"])
        image = Null.wx_call(:wxBitmap, :convertToImage, [fallback])
        Null.wx_call(:wxBitmap, :destroy, [fallback])
        image
      end

    {:ok, image}
  end

  @impl true
  def new_icon(app, path) do
    with {:ok, image} <- load_image(app, path), do: new_icon_from(image)
  end

  @impl true
  def new_icon_from(image) do
    case object_type(image) do
      :wxImage ->
        bitmap = Null.wx_call(:wxBitmap, :new, [image])
        icon = Null.wx_call(:wxIcon, :new, [])
        Null.wx_call(:wxIcon, :copyFromBitmap, [icon, bitmap])
        media_destroy(bitmap)
        {:ok, icon}

      :wxBitmap ->
        icon = Null.wx_call(:wxIcon, :new, [])
        Null.wx_call(:wxIcon, :copyFromBitmap, [icon, image])
        {:ok, icon}

      :wxIcon ->
        {:ok, image}
    end
  end

  @impl true
  def default_icon, do: {:ok, Null.wx_call(:wxArtProvider, :getIcon, ["wxART_EXECUTABLE_FILE"])}

  @impl true
  def media_destroy(image) do
    module = object_type(image)
    Null.wx_call(module, :destroy, [image])
    :ok
  end

  @impl true
  def object_type(image), do: Null.wx_call(:wx, :getObjectType, [image])

  defp check_has_webview do
    if Null.module?(:wxWebView), do: :ok, else: {:error, :no_webview}
  end

  defp clear_windows(frame) do
    sizer = Null.wx_call(:wxFrame, :getSizer, [frame])
    Null.wx_call(:wxSizer, :clear, [sizer, [delete_windows: true]])
    sizer
  end

  defp backend_available?(backend) do
    try do
      Null.wx_call(:wxWebView, :isBackendAvailable, [String.to_charlist(backend)])
    rescue
      _ -> false
    end
  end

  defp do_webview_new(frame) do
    env = System.get_env("WX_WEBVIEW_BACKEND", "none")

    cond do
      backend_available?(env) ->
        do_webview_new(frame, backend: String.to_charlist(env))

      backend_available?("wxWebViewChromium") ->
        do_webview_new(frame, backend: ~c"wxWebViewChromium")

      backend_available?("wxWebViewEdge") ->
        do_webview_new(frame, backend: ~c"wxWebViewEdge")

      OS.type() == Windows ->
        {:error, :missing_edge}

      true ->
        do_webview_new(frame, [])
    end
  end

  defp do_webview_new(frame, opts) do
    put_webview_backend(Keyword.get(opts, :backend, "default") |> to_string())

    try do
      {:ok, Null.wx_call(:wxWebView, :new, [frame, -1, [{:style, Wx.wxNO_BORDER()} | opts]])}
    rescue
      _ -> {:error, :no_webview}
    end
  end

  defp notification_events_available? do
    {Wx.wxMAJOR_VERSION(), Wx.wxMINOR_VERSION(), Wx.wxRELEASE_NUMBER()}
    |> case do
      {major, minor, _} when major >= 3 and minor >= 1 -> true
      _ -> false
    end
  end

  defp get_abs_path(_, "/" <> path), do: path
  defp get_abs_path(app, name), do: Application.app_dir(app, ["priv", name])
end
