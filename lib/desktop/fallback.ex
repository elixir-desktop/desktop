defmodule Desktop.Fallback do
  require Logger
  alias Desktop.{Wx, OS}

  def webview_new(frame) do
    if is_module?(:wxWebView) do
      sizer = :wxFrame.getSizer(frame)
      :wxSizer.clear(sizer, delete_windows: true)

      webview =
        if OS.type() == Windows do
          if not :wxWebView.isBackendAvailable('wxWebViewEdge') do
            Logger.warning("""
            Missing support for wxWebViewEdge.
            Check your OTP install for edge support and download it here:
            https://go.microsoft.com/fwlink/p/?LinkId=2124703
            """)

            win = :wxHtmlWindow.new(frame, [])

            :wxHtmlWindow.setPage(win, """
              <html>
                <body>
                  <h1>Missing Edge Runtime</h1>
                  <p>This demo requires the edge runtime to be installed</p>
                  <p>Please download it <a href="https://go.microsoft.com/fwlink/p/?LinkId=2124703">here</a> and try again</p>
                  <p>
                    <a href="https://go.microsoft.com/fwlink/p/?LinkId=2124703">https://go.microsoft.com/fwlink/p/?LinkId=2124703</a>
                  </p>
                </body>
              </html>
            """)

            :wxHtmlWindow.connect(win, :command_html_link_clicked, skip: true)
            win
          else
            :wxWebView.new(frame, -1, backend: 'wxWebViewEdge')
            |> configure_webview()
          end
        else
          :wxWebView.new(frame, -1)
          |> configure_webview()
        end

      :wxBoxSizer.add(sizer, webview, proportion: 1, flag: Wx.wxEXPAND())
      :wxSizer.layout(sizer)
      :wxSizer.show(sizer, true)
      :wxFrame.refresh(frame)
      webview
    else
      Logger.warning(
        "Missing support for wxWebView - upgrade to OTP/24. Will show OS browser instead"
      )
    end
  end

  defp configure_webview(webview) do
    :wxWebView.connect(webview, :webview_newwindow)
    :wxWebView.enableContextMenu(webview, enable: false)
    webview
  end

  def webview_can_fix(webview) do
    is_module?(:wxWebView) and OS.type() == Windows and
      :wxWebView.isBackendAvailable('wxWebViewEdge') and :wxWebView.isShownOnScreen(webview)
  end

  def webview_show(%Desktop.Window{webview: webview, frame: frame}, url, default) do
    if is_module?(:wxWebView) and
         (OS.type() != Windows or :wxWebView.isBackendAvailable('wxWebViewEdge')) do
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

  def webview_rebuild(%Desktop.Window{webview: webview, frame: frame, last_url: url}) do
    if is_module?(:wxWebView) and
         (OS.type() != Windows or :wxWebView.isBackendAvailable('wxWebViewEdge')) do
      # url = :wxWebView.getCurrentURL(webview)
      webview = webview_new(frame)
      Logger.info("Rebuilding WebView on Windows with url: #{inspect(url)}")

      if url != nil do
        :wxWebView.loadURL(webview, url)
      end

      webview
    else
      webview
    end
  end

  def notification_new(title, type) do
    if is_module?(:wxNotificationMessage) do
      flag =
        case type do
          :info -> Wx.wxICON_INFORMATION()
          :warning -> Wx.wxICON_WARNING()
          :error -> Wx.wxICON_ERROR()
        end

      notification = :wxNotificationMessage.new(title, flags: flag)

      if notification_events_available?() do
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

  def notification_show(notification, message, timeout) do
    if is_module?(:wxNotificationMessage) do
      :wxNotificationMessage.setMessage(notification, to_charlist(message))
      :wxNotificationMessage.show(notification, timeout: timeout)
    else
      Logger.notice("NOTIFICATION: #{message}")
    end
  end

  defp is_module?(module) do
    Code.ensure_compiled(module) == {:module, module}
  end

  defp notification_events_available?() do
    {Wx.wxMAJOR_VERSION(), Wx.wxMINOR_VERSION(), Wx.wxRELEASE_NUMBER()}
    |> case do
      {major, minor, _} when major >= 3 and minor >= 1 -> true
      _ -> false
    end
  end
end
