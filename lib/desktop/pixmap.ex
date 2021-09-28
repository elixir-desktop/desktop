defmodule Desktop.Pixmap do
  @moduledoc """
  Module that generates pixmaps out of wx objects
  such as :wxIcon, :wxImage, :wxBitmap
  """

  def from_wx_icon(icon) do
    width = :wxIcon.getWidth(icon)
    height = :wxIcon.getHeight(icon)
    bitmap = :wxBitmap.new(width, height)

    if :wxBitmap.copyFromIcon(bitmap, icon) do
      ret = from_wx_bitmap(bitmap)
      :wxBitmap.destroy(bitmap)
      ret
    else
      :wxBitmap.destroy(bitmap)
      {:error, "Failed to copy bitmap from icon"}
    end
  end

  def from_wx_icon(icon, env) do
    :wx.set_env(env)
    from_wx_icon(icon)
  end

  defp from_wx_bitmap(bitmap) do
    width = :wxBitmap.getWidth(bitmap)
    height = :wxBitmap.getHeight(bitmap)
    image = :wxBitmap.convertToImage(bitmap)

    rgb = :wxImage.getData(image)
    alpha = :wxImage.getAlpha(image)

    :wxImage.destroy(image)

    argb = to_argb(alpha, rgb)

    pixmap_data = [{width, height, argb}]
    {:ok, pixmap_data}
  end

  defp to_argb(<<>>, <<>>) do
    <<>>
  end

  defp to_argb(
         <<a, alpha::binary>>,
         <<r, g, b, rgb::binary>>
       ) do
    <<a, r, g, b>> <> to_argb(alpha, rgb)
  end
end
