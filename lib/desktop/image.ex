defmodule Desktop.Image do
  require Logger
  @moduledoc false
  def new(app, path) when is_binary(path) do
    image = :wxImage.new(get_abs_path(app, path))

    image =
      if :wxImage.isOk(image) do
        image
      else
        Logger.error("Could not load image #{get_abs_path(app, path)}")
        fallback = :wxArtProvider.getBitmap("wxART_ERROR")
        image = :wxBitmap.convertToImage(fallback)
        :wxBitmap.destroy(fallback)
        image
      end

    {:ok, image}
  end

  def new_icon(app, path) do
    {:ok, image} = new(app, path)
    new_icon(image)
  end

  def new_icon(image) do
    case :wx.getObjectType(image) do
      :wxImage ->
        bitmap = :wxBitmap.new(image)
        icon = :wxIcon.new()
        :ok = :wxIcon.copyFromBitmap(icon, bitmap)
        destroy(bitmap)
        {:ok, icon}

      :wxBitmap ->
        icon = :wxIcon.new()
        :ok = :wxIcon.copyFromBitmap(icon, image)
        {:ok, icon}

      :wxIcon ->
        {:ok, image}
    end
  end

  def destroy(image) do
    module = :wx.getObjectType(image)
    module.destroy(image)
  end

  defp get_abs_path(_, abs_path = "/" <> _) do
    abs_path
  end

  defp get_abs_path(app, name) when is_binary(name) do
    Application.app_dir(app, ["priv", name])
  end
end
