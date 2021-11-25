defmodule Desktop.Pixmap do
  @moduledoc false

  alias Desktop.Wx

  def from_wx_icon(icon, opts \\ []) do
    env = Keyword.get(opts, :env, nil)

    if env != nil do
      :wx.set_env(env)
    end

    width = :wxIcon.getWidth(icon)
    height = :wxIcon.getHeight(icon)
    bitmap = :wxBitmap.new(width, height)

    if :wxBitmap.copyFromIcon(bitmap, icon) do
      ret = from_wx_bitmap(bitmap, opts)
      :wxBitmap.destroy(bitmap)
      ret
    else
      :wxBitmap.destroy(bitmap)
      {:error, "Failed to copy bitmap from icon"}
    end
  end

  defp from_wx_bitmap(bitmap, opts) do
    pixmap_from_wx_bitmap(bitmap, opts)
  end

  defp pixmap_from_wx_bitmap(bitmap, opts) do
    width = :wxBitmap.getWidth(bitmap)
    height = :wxBitmap.getHeight(bitmap)
    image = :wxBitmap.convertToImage(bitmap)

    # Rescaling happens in place, meaning it changes the memory data
    # inside the wxImage ref
    resize_width = Keyword.get(opts, :width, width)
    resize_height = Keyword.get(opts, :height, height)
    rescale? = Keyword.get(opts, :rescale, false)

    {width, height, image} =
      if rescale? and (width != resize_width or height != resize_height) do
        rescale_image(image, resize_width, resize_height)
      else
        {width, height, image}
      end

    argb = argb_from_wx_image(image)

    :wxImage.destroy(image)

    {:ok, {width, height, argb}}
  end

  defp rescale_image(image, width, height) do
    image = :wxImage.rescale(image, width, height, quality: Wx.wxIMAGE_QUALITY_HIGH())
    {width, height, image}
  end

  defp argb_from_wx_image(image) do
    rgb = :wxImage.getData(image)
    alpha = :wxImage.getAlpha(image)

    to_argb(alpha, rgb)
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
