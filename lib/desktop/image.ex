defmodule Desktop.Image do
  require Logger
  @moduledoc false

  alias Desktop.Platform.Media

  def new(app, path), do: Media.load_image(app, path)

  def new_icon(app, path), do: Media.new_icon(app, path)

  def new_icon(image), do: Media.new_icon_from(image)

  def destroy(image), do: Media.destroy(image)
end
