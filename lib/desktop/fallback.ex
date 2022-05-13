defmodule Desktop.Fallback do
  require Logger
  alias Desktop.{Wx, OS}

  @moduledoc """
    Fallback handles version differences in the :wx modules needed for showing the
    WebView and Desktop notifications and it uses the highest available
    feature level while trying to stays backwards compatible to older :wx versions.
  """

  def webview_new(frame) do
    with :ok <- check_has_webview(),
         sizer <- clear_windows(frame),
         {:ok, webview} <- webview_new_by_os_type(frame, OS.type()) do
      call(:wxWebView, :connect, [webview, :webview_newwindow])
      call(:wxWebView, :enableContextMenu, [webview, [enable: false]])

      :wxBoxSizer.add(sizer, webview, proportion: 1, flag: Wx.wxEXPAND())
      :wxSizer.layout(sizer)
      :wxSizer.show(sizer, true)
      :wxFrame.refresh(frame)
      webview
    else
      {:error, reason} ->
        Logger.warning(reason)
        nil
    end
  end

  defp check_has_webview() do
    if is_module?(:wxWebView) do
      :ok
    else
      {:error, "Missing support for wxWebView - upgrade to OTP/24. Will show OS browser instead"}
    end
  end

  defp clear_windows(frame) do
    sizer = :wxFrame.getSizer(frame)
    :wxSizer.clear(sizer, delete_windows: true)
    sizer
  end

  defp webview_new_by_os_type(frame, Windows) do
    if call(:wxWebView, :isBackendAvailable, ['wxWebViewEdge']) do
      {:ok,
       call(:wxWebView, :new, [
         frame,
         -1,
         [backend: 'wxWebViewEdge', style: Desktop.Wx.wxNO_BORDER()]
       ])}
    else
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

      {:error,
       """
       Missing support for wxWebViewEdge.
       Check your OTP install for edge support and download it here:
       https://go.microsoft.com/fwlink/p/?LinkId=2124703
       """}
    end
  end

  defp webview_new_by_os_type(frame, _) do
    {:ok, call(:wxWebView, :new, [frame, -1, [style: Desktop.Wx.wxNO_BORDER()]])}
  end

  def webview_can_fix(nil), do: false

  def webview_can_fix(webview) do
    is_module?(:wxWebView) and OS.type() == Windows and
      call(:wxWebView, :isBackendAvailable, ['wxWebViewEdge']) and
      call(:wxWebView, :isShownOnScreen, [webview])
  end

  def webview_url(%Desktop.Window{webview: nil, last_url: last_url}), do: last_url

  def webview_url(%Desktop.Window{webview: webview}) do
    call(:wxWebView, :getCurrentURL, [webview])
  end

  def webview_show(%Desktop.Window{webview: nil}, url, _) do
    :wx_misc.launchDefaultBrowser(url)
  end

  def webview_show(
        %Desktop.Window{webview: webview, frame: frame, last_url: last},
        url,
        only_open
      ) do
    if last == nil or not only_open do
      call(:wxWebView, :loadURL, [webview, url])
    end

    if :wxTopLevelWindow.isIconized(frame) do
      :wxTopLevelWindow.iconize(frame, iconize: false)
    end

    if not :wxWindow.isShown(frame) do
      :wxWindow.show(frame, show: true)
      :wxTopLevelWindow.centerOnScreen(frame)
    end

    OS.raise_frame(frame)
  end

  def webview_rebuild(%Desktop.Window{webview: nil}), do: nil

  def webview_rebuild(%Desktop.Window{frame: frame, last_url: url}) do
    # url = call(:wxWebView, :getCurrentURL, [webview])
    webview = webview_new(frame)
    Logger.info("Rebuilding WebView on Windows with url: #{inspect(url)}")

    if url != nil do
      call(:wxWebView, :loadURL, [webview, url])
    end

    webview
  end

  def notification_new(title, type) do
    if is_module?(:wxNotificationMessage) do
      flag =
        case type do
          :info -> Wx.wxICON_INFORMATION()
          :warning -> Wx.wxICON_WARNING()
          :error -> Wx.wxICON_ERROR()
        end

      notification = call(:wxNotificationMessage, :new, [title, [flags: flag]])

      if notification_events_available?() do
        for event <- [
              :notification_message_click,
              :notification_message_dismissed,
              :notification_message_action
            ] do
          call(:wxNotificationMessage, :connect, [notification, event])
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

  def notification_show(notification, message, timeout, title \\ nil) do
    if is_module?(:wxNotificationMessage) do
      if title != nil do
        call(:wxNotificationMessage, :setTitle, [notification, to_charlist(title)])
      end

      call(:wxNotificationMessage, :setMessage, [notification, to_charlist(message)])
      call(:wxNotificationMessage, :show, [notification, [timeout: timeout]])
    else
      Logger.notice("NOTIFICATION: #{title}: #{message}")
    end
  end

  def wx_subscribe() do
    call(:wx, :subscribe_events)
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

  defp call(module, method, args \\ []) do
    if Kernel.function_exported?(module, method, length(args)) do
      apply(module, method, args)
    end
  end
end
