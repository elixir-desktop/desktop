defmodule Desktop.Image do
  def new(app, path) when is_binary(path) do
    path = get_abs_path(app, path)
    {:ok, :wxImage.new(path)}
  end

  def new_icon(app, path) do
    with {:ok, image} = new(app, path) do
      new_icon(image)
    end
  end

  def new_icon(image = {:wx_ref, _, :wxImage, _}) do
    bitmap = :wxBitmap.new(image)

    ret = new_icon(bitmap)
    destroy(bitmap)
    ret
  end

  def new_icon(bitmap = {:wx_ref, _, :wxBitmap, _}) do
    icon = :wxIcon.new()

    case :wxIcon.copyFromBitmap(icon, bitmap) do
      :ok ->
        {:ok, icon}

      error ->
        destroy(icon)
        error
    end
  end

  def new_icon(icon = {:wx_ref, _, :wxIcon, _}) do
    icon
  end

  def destroy(src = {:wx_ref, _, :wxImage, _}) do
    :wxImage.destroy(src)
  end

  def destroy(src = {:wx_ref, _, :wxBitmap, _}) do
    :wxBitmap.destroy(src)
  end

  def destroy(src = {:wx_ref, _, :wxIcon, _}) do
    :wxIcon.destroy(src)
  end

  defp get_abs_path(_, abs_path = "/" <> _) do
    abs_path
  end

  defp get_abs_path(app, name) when is_binary(name) do
    Application.app_dir(app, ["priv", name])
  end
end
