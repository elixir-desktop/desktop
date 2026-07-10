defmodule Desktop.Backend.Json do
  @moduledoc false

  alias Desktop.Bridge.{Protocol, Transport}
  alias Desktop.Wx

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
      content: :native,
      notification: :native,
      menu: :native,
      taskbar: false
    }
  end

  # System

  @impl true
  def init_env do
    wx = Transport.ensure_started()
    {wx, :ok}
  end

  @impl true
  def subscribe_events do
    Transport.subscribe_events(self())
    :ok
  end

  @impl true
  def set_env(_env), do: :ok

  @impl true
  def get_env, do: :ok

  @impl true
  def locale do
    # Pass :getSystemLanguage as a bridge atom — never call :wxLocale.getSystemLanguage/0
    # on the BEAM (that module is not loaded on Android/iOS releases).
    locale =
      Protocol.new(:wxLocale, [:getSystemLanguage])

    case Protocol.call(:wxLocale, :getCanonicalName, [locale]) do
      name when is_list(name) -> List.to_string(name)
      name when is_binary(name) -> name
      _ -> nil
    end
    |> case do
      nil -> nil
      code -> String.downcase(code)
    end
  end

  @impl true
  def connect_menu(object, command, callback, id) do
    opts = [{:callback, fn _, _ -> callback.() end}]
    opts = if id == nil, do: opts, else: [{:id, id} | opts]
    Protocol.connect(:wxMenu, object, command, opts)
    :ok
  end

  @impl true
  def wx_available?, do: false

  @impl true
  def open_external_url(url) do
    Protocol.call(:wx_misc, :launchDefaultBrowser, [String.to_charlist(url)])
    :ok
  end

  @impl true
  def activate_event_active?(_event), do: true

  # Window

  @impl true
  def open(opts) do
    wx = Keyword.fetch!(opts, :wx)
    title = Keyword.fetch!(opts, :title)
    size = Keyword.get(opts, :size, {600, 500})

    frame =
      Protocol.new(:wxFrame, [
        wx,
        Wx.wxID_ANY(),
        title,
        [{:size, size}, {:style, Wx.wxDEFAULT_FRAME_STYLE()}]
      ])

    min_size = Keyword.get(opts, :min_size)
    if min_size, do: Protocol.call(:wxFrame, :setMinSize, [frame, min_size])

    Protocol.call(:wxFrame, :setSizer, [
      frame,
      Protocol.new(:wxBoxSizer, [Wx.wxHORIZONTAL()])
    ])

    icon = Keyword.get(opts, :icon)

    {:ok, icon} =
      case icon do
        nil -> Desktop.Platform.Media.default_icon()
        other -> {:ok, other}
      end

    Protocol.call(:wxTopLevelWindow, :setIcon, [frame, icon])
    webview = Desktop.Platform.Content.attach(frame)
    {:ok, frame, webview}
  end

  @impl true
  def destroy_frame(frame), do: Protocol.destroy(:wxFrame, frame)

  @impl true
  def connect(frame, event, fun) do
    Protocol.connect(:wxFrame, frame, event, callback: fun, userData: self())
    :ok
  end

  @impl true
  def show(frame, opts) do
    Protocol.call(:wxFrame, :show, [frame, [show: Keyword.get(opts, :show, true)]])
    :ok
  end

  @impl true
  def hide(frame) do
    Protocol.call(:wxFrame, :hide, [frame])
    :ok
  end

  @impl true
  def set_title(frame, title) do
    Protocol.call(:wxFrame, :setTitle, [frame, String.to_charlist(title)])
    :ok
  end

  @impl true
  def set_min_size(frame, size), do: Protocol.call(:wxFrame, :setMinSize, [frame, size]) || :ok

  @impl true
  def set_icon(frame, icon) do
    Protocol.call(:wxTopLevelWindow, :setIcon, [frame, icon])
    :ok
  end

  @impl true
  def set_menubar(frame, menubar) do
    Protocol.call(:wxFrame, :setMenuBar, [frame, menubar])
    :ok
  end

  @impl true
  def iconize(frame, iconize) do
    Protocol.call(:wxTopLevelWindow, :iconize, [frame, [iconize: iconize]])
    :ok
  end

  @impl true
  def shown?(frame), do: Protocol.call(:wxWindow, :isShown, [frame]) || false

  @impl true
  def active?(frame), do: Protocol.call(:wxTopLevelWindow, :isActive, [frame]) || true

  @impl true
  def raise_window(nil), do: :ok

  def raise_window(frame) do
    Protocol.call(:wxTopLevelWindow, :setFocus, [frame])
    Protocol.call(:wxWindow, :raise, [frame])
    :ok
  end

  @impl true
  def update_apple_menu(_title, _frame, _menubar), do: :ok

  @impl true
  def new_menubar, do: Protocol.new(:wxMenuBar, [])

  @impl true
  def on_crash_destroy(frame), do: destroy_frame(frame)

  @impl true
  def close_event_veto(_inev), do: :ok

  # Content

  @impl true
  def attach(frame) do
    webview =
      Protocol.new(:wxWebView, [frame, -1, [style: Wx.wxNO_BORDER()]])

    Protocol.connect(:wxWebView, webview, :webview_newwindow)
    Protocol.connect(:wxWebView, webview, :webview_error)
    Protocol.call(:wxWebView, :enableContextMenu, [webview, [enable: false]])
    webview
  end

  @impl true
  def load_url(nil, _frame, url), do: open_external_url(url)

  def load_url(webview, _frame, url) do
    Protocol.call(:wxWebView, :loadURL, [webview, url])
    :ok
  end

  @impl true
  def current_url(nil, last_url), do: last_url

  def current_url(webview, _last_url) do
    case Protocol.call(:wxWebView, :getCurrentURL, [webview]) do
      url when is_list(url) -> List.to_string(url)
      other -> other
    end
  end

  @impl true
  def content_show(nil, _frame, url, _), do: open_external_url(url)

  def content_show(webview, frame, url, _only_open) do
    if url, do: Protocol.call(:wxWebView, :loadURL, [webview, url])
    Protocol.call(:wxFrame, :show, [frame, [show: true]])
    Protocol.call(:wxTopLevelWindow, :centerOnScreen, [frame])
    :ok
  end

  @impl true
  def rebuild(nil, _url), do: nil

  def rebuild(frame, url) do
    webview = attach(frame)
    if url, do: Protocol.call(:wxWebView, :loadURL, [webview, url])
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
    flag =
      case type do
        :info -> Wx.wxICON_INFORMATION()
        :warning -> Wx.wxICON_WARNING()
        :error -> Wx.wxICON_ERROR()
      end

    Protocol.new(:wxNotificationMessage, [title, [flags: flag]])
  end

  @impl true
  def notification_show(nil, message, _timeout, title) do
    require Logger
    Logger.notice("NOTIFICATION: #{title}: #{message}")
    :ok
  end

  def notification_show(notification, message, timeout, title) do
    if title,
      do: Protocol.call(:wxNotificationMessage, :setTitle, [notification, to_charlist(title)])

    Protocol.call(:wxNotificationMessage, :setMessage, [notification, to_charlist(message)])
    Protocol.call(:wxNotificationMessage, :show, [notification, [timeout: timeout]])
    :ok
  end

  @impl true
  def close(notification) do
    Protocol.call(:wxNotificationMessage, :close, [notification])
    :ok
  end

  # Media

  @impl true
  def load_image(app, path) do
    image = Protocol.new(:wxImage, [get_abs_path(app, path)])
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
        bitmap = Protocol.new(:wxBitmap, [image])
        icon = Protocol.new(:wxIcon, [])
        Protocol.call(:wxIcon, :copyFromBitmap, [icon, bitmap])
        media_destroy(bitmap)
        {:ok, icon}

      :wxIcon ->
        {:ok, image}

      _ ->
        {:ok, image}
    end
  end

  @impl true
  def default_icon, do: {:ok, Protocol.call(:wxArtProvider, :getIcon, ["wxART_EXECUTABLE_FILE"])}

  @impl true
  def media_destroy(image) do
    type = object_type(image)
    Protocol.destroy(type, image)
    :ok
  end

  @impl true
  def object_type(image), do: Transport.bridge_call(:wx, :getObjectType, [image])

  defp get_abs_path(_, "/" <> path), do: path
  defp get_abs_path(app, name), do: Application.app_dir(app, ["priv", name])
end
