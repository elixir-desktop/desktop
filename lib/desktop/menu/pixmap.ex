defmodule Desktop.Menu.Pixmap do
  def from_wxIcon(icon) do
    width = :wxIcon.getWidth(icon)
    height = :wxIcon.getHeight(icon)
    bitmap = :wxBitmap.new(width, height)

    if :wxBitmap.copyFromIcon(bitmap, icon) do
      ret = from_wxBitmap(bitmap)
      :wxBitmap.destroy(bitmap)
      ret
    else
      :wxBitmap.destroy(bitmap)
      {:error, "Failed to copy bitmap from icon"}
    end
  end

  def from_wxIcon(icon, env) do
    :wx.set_env(env)
    from_wxIcon(icon)
  end

  defp from_wxBitmap(bitmap) do
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
