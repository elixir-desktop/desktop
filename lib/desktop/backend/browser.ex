defmodule Desktop.Backend.Browser do
  @moduledoc false

  @behaviour Desktop.Platform.Backend
  @behaviour Desktop.Platform.Window
  @behaviour Desktop.Platform.Content
  @behaviour Desktop.Platform.Notification
  @behaviour Desktop.Platform.Media
  @behaviour Desktop.Platform.System

  @impl true
  def capabilities do
    %{
      window: false,
      content: :os_browser,
      notification: :log,
      menu: :none,
      taskbar: false
    }
  end

  # System

  @impl true
  def init_env, do: {nil, nil}

  @impl true
  def subscribe_events, do: :ok

  @impl true
  def set_env(_env), do: :ok

  @impl true
  def get_env, do: nil

  @impl true
  def locale, do: nil

  @impl true
  def connect_menu(_object, _command, _callback, _id), do: :ok

  @impl true
  def wx_available?, do: false

  @impl true
  def open_external_url(url), do: Desktop.Impl.HostBrowser.open(url)

  @impl true
  def os_description, do: nil

  @impl true
  def custom_event(_event, _args), do: :ok

  @impl true
  def activate_event_active?(_event), do: true

  @impl true
  def prepare_shutdown, do: :ok

  # Window

  @impl true
  def open(_opts), do: {:ok, nil, nil}

  @impl true
  def destroy_frame(_frame), do: :ok

  @impl true
  def connect(_frame, _event, _fun), do: :ok

  @impl true
  def show(_frame, _opts), do: :ok

  @impl true
  def hide(_frame), do: :ok

  @impl true
  def set_title(_frame, _title), do: :ok

  @impl true
  def set_min_size(_frame, _size), do: :ok

  @impl true
  def set_icon(_frame, _icon), do: :ok

  @impl true
  def set_menubar(_frame, _menubar), do: :ok

  @impl true
  def iconize(_frame, _iconize), do: :ok

  @impl true
  def shown?(_frame), do: false

  @impl true
  def active?(_frame), do: false

  @impl true
  def raise_window(_frame), do: :ok

  @impl true
  def update_apple_menu(_title, _frame, _menubar), do: :ok

  @impl true
  def new_menubar, do: nil

  @impl true
  def on_crash_destroy(_frame), do: :ok

  @impl true
  def close_event_veto(_inev), do: :ok

  # Content

  @impl true
  def attach(_frame), do: nil

  @impl true
  def load_url(_content, _frame, url), do: open_external_url(url)

  @impl true
  def current_url(_content, last_url), do: last_url

  @impl true
  def content_show(_content, _frame, url, _), do: open_external_url(url)

  @impl true
  def rebuild(_frame, _url), do: nil

  @impl true
  def reload(_content), do: :ok

  @impl true
  def put_webview_backend(name) do
    Desktop.Env.put(:webview_backend, name)
    :ok
  end

  # Notification

  @impl true
  def new(_title, _type), do: nil

  @impl true
  def notification_show(nil, message, _timeout, title) do
    require Logger
    Logger.notice("NOTIFICATION: #{title}: #{message}")
    :ok
  end

  def notification_show(_notification, message, _timeout, title) do
    notification_show(nil, message, 0, title)
  end

  @impl true
  def close(_notification), do: :ok

  # Media

  @impl true
  def load_image(_app, _path), do: {:error, :unsupported}

  @impl true
  def new_icon(_app, _path), do: {:error, :unsupported}

  @impl true
  def new_icon_from(_image), do: {:error, :unsupported}

  @impl true
  def default_icon, do: {:ok, nil}

  @impl true
  def media_destroy(_image), do: :ok

  @impl true
  def object_type(_image), do: :unknown
end
