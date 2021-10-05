defmodule Desktop.Wx.TaskBarIcon do
  require Logger

  alias Desktop.OS

  defstruct fn_create_popup: nil,
            skip_popup_menu?: true,
            wx_taskbar_icon: nil,
            wx_menu: nil

  @type wx_menu() :: any()
  @type wx_taskbar_icon() :: any()
  @type create_popup() :: (() -> wx_menu())
  @type t() :: %__MODULE__{
          fn_create_popup: nil | create_popup(),
          skip_popup_menu?: boolean(),
          wx_taskbar_icon: wx_taskbar_icon(),
          wx_menu: wx_menu()
        }

  @spec create(fn_create_popup :: create_popup()) :: {:ok, t()} | {:error, any()}
  def create(fn_create_popup) when is_function(fn_create_popup) do
    taskbar_icon = %__MODULE__{
      fn_create_popup: fn_create_popup,
      skip_popup_menu?: true
    }

    with {:ok, taskbar_icon = %{wx_taskbar_icon: wx_taskbar_icon}} <-
           create(taskbar_icon, OS.type()) do
      OnCrash.call(fn ->
        :wx.set_env(Desktop.Env.wx_env())
        :wxTaskBarIcon.removeIcon(wx_taskbar_icon)
        :wxTaskBarIcon.destroy(wx_taskbar_icon)
      end)

      {:ok, taskbar_icon}
    end
  end

  @spec create(t(), os_type :: atom()) :: {:ok, t()} | {:error, any()}
  def create(taskbar_icon = %__MODULE__{fn_create_popup: fn_create_popup}, MacOS)
      when is_function(fn_create_popup) do
    with {:ok, wx_taskbar_icon} <- create_taskbar_icon_osx(fn_create_popup) do
      {:ok,
       %{
         taskbar_icon
         | wx_taskbar_icon: wx_taskbar_icon,
           # On MacOS we always skip using popupMenu() method
           skip_popup_menu?: true
       }}
    end
  end

  def create(taskbar_icon = %__MODULE__{fn_create_popup: fn_create_popup}, _) do
    case create_taskbar_icon(createPopupMenu: fn_create_popup) do
      {:ok, wx_taskbar_icon} ->
        {:ok, %{taskbar_icon | wx_taskbar_icon: wx_taskbar_icon}}

      _ ->
        # Failed to create with :wxTaskBarIcon.new/1
        # Fallback to :wxTaskBarIcon.new/0
        case create_taskbar_icon_fallback() do
          {:ok, wx_taskbar_icon} ->
            # When using the fallback, enable popupMenu()
            # by setting skip_popup_menu? to false
            {:ok, %{taskbar_icon | wx_taskbar_icon: wx_taskbar_icon, skip_popup_menu?: false}}

          error ->
            error
        end
    end
  end

  @spec popupMenu(t()) :: boolean()
  def popupMenu(%__MODULE__{
        wx_taskbar_icon: wx_taskbar_icon,
        fn_create_popup: fn_create_popup,
        skip_popup_menu?: false
      })
      when is_function(fn_create_popup) do
    menu = fn_create_popup.()
    :wxTaskBarIcon.popupMenu(wx_taskbar_icon, menu)
  end

  def popupMenu(%__MODULE__{
        fn_create_popup: fn_create_popup
      })
      when is_function(fn_create_popup) do
    true
  end

  def popupMenu(_) do
    false
  end

  def setIcon(%__MODULE__{wx_taskbar_icon: wx_taskbar_icon}, icon) do
    :wxTaskBarIcon.setIcon(wx_taskbar_icon, icon)
  end

  def setIcon(%__MODULE__{wx_taskbar_icon: wx_taskbar_icon}, icon, options) do
    :wxTaskBarIcon.setIcon(wx_taskbar_icon, icon, options)
  end

  def removeIcon(%__MODULE__{wx_taskbar_icon: wx_taskbar_icon}) do
    :wxTaskBarIcon.removeIcon(wx_taskbar_icon)
  end

  def connect(%__MODULE__{wx_taskbar_icon: wx_taskbar_icon}) do
    :wxTaskBarIcon.connect(wx_taskbar_icon, :taskbar_left_down, skip: true)
    :wxTaskBarIcon.connect(wx_taskbar_icon, :taskbar_right_down, skip: true)
  end

  defp create_taskbar_icon_osx(fn_create_popup) do
    if supports_method?(:new, 1) do
      # Supports :wxTaskBarIcon.new(opts)

      if is_module?(:wxWebView) do
        # Proper OTP24 release
        create_taskbar_icon(createPopupMenu: fn_create_popup)
      else
        # Pre-OTP24 custom version for backwards compat.
        try do
          :wxTaskBarIcon.new(fn_create_popup)
        catch
          :error, :function_clause ->
            Logger.error("No MacOS compatible :wxTaskBarIcon found! Please use at least OTP24")

            create_taskbar_icon_fallback()
        else
          wx_taskbar_icon -> {:ok, wx_taskbar_icon}
        end
      end
    else
      # Does not support constructor with 1 argument
      create_taskbar_icon_fallback()
    end
  end

  defp create_taskbar_icon(opts) do
    if supports_method?(:new, 1) do
      try do
        :wxTaskBarIcon.new(opts)
      rescue
        e -> {:error, e}
      else
        wx_taskbar_icon -> {:ok, wx_taskbar_icon}
      end
    else
      {:error, ":wxTaskBarIcon.new/1 not available"}
    end
  end

  defp create_taskbar_icon_fallback() do
    if supports_method?(:new, 0) do
      try do
        :wxTaskBarIcon.new()
      rescue
        e -> {:error, e}
      else
        wx_taskbar_icon -> {:ok, wx_taskbar_icon}
      end
    else
      {:error, ":wxTaskBarIcon.new/0 not available"}
    end
  end

  defp supports_method?(method, arity) do
    if is_module?(:wxTaskBarIcon) do
      Kernel.function_exported?(:wxTaskBarIcon, method, arity)
    else
      false
    end
  end

  defp is_module?(module) do
    Code.ensure_loaded?(module)
  end
end
