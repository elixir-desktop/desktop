defmodule Desktop.Fallback do
  require Logger
  alias Desktop.{Wx, OS}

  def webview_new(frame) do
    if is_module?(:wxWebView) do
      sizer = :wxBoxSizer.new(Wx.wxHORIZONTAL())
      webview = :wxWebView.new(frame, -1)
      :wxWebView.connect(webview, :webview_newwindow)
      :wxWebView.enableContextMenu(webview, enable: false)
      :wxBoxSizer.add(sizer, webview, proportion: 1, flag: Wx.wxEXPAND())
      :wxFrame.setSizer(frame, sizer)
      webview
    else
      Logger.warning(
        "Missing support for wxWebView - upgrade to OTP/24. Will show OS browser instead"
      )
    end
  end

  def webview_show(%Desktop.Window{webview: webview, frame: frame}, url, default) do
    if is_module?(:wxWebView) do
      if url != nil do
        :wxWebView.loadURL(webview, url)
      end

      :wxWindow.show(frame, show: true)
      :wxTopLevelWindow.centerOnScreen(frame)
      OS.raise_frame(frame)
    else
      :wx_misc.launchDefaultBrowser(url || default)
    end
  end

  def notification_new(title, type) do
    if is_module?(:wxNotificationMessage) do
      flag =
        case type do
          :info -> Wx.wxICON_INFORMATION()
          :warn -> Wx.wxICON_WARNING()
          :warning -> Wx.wxICON_WARNING()
          :error -> Wx.wxICON_ERROR()
        end

      notification = :wxNotificationMessage.new(title, flags: flag)

      if :desktop_fallback.notification_events_available() do
        for event <- [
              :notification_message_click,
              :notification_message_dismissed,
              :notification_message_action
            ] do
          :wxNotificationMessage.connect(notification, event)
        end
      else
        Logger.warning(
          "Missing support for wxNotificationMessage Events - upgrade to wxWidgets 3.1 - messages won't be clickable"
        )
      end

      notification
    else
      Logger.warning(
        "Missing support for wxNotificationMessage - upgrade to OTP/24. Messages will be logged only"
      )
    end
  end

  def notification_show(notification, message) do
    if is_module?(:wxNotificationMessage) do
      :wxNotificationMessage.setMessage(notification, to_charlist(message))
      :wxNotificationMessage.show(notification)
    else
      Logger.notice("NOTIFICATION: #{message}")
    end
  end

  def is_module?(module) do
    Code.ensure_compiled(module) == {:module, module}
  end
end
