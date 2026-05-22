defmodule Desktop.Platform.Media do
  @moduledoc false

  @callback load_image(app :: atom(), path :: String.t()) :: {:ok, term()} | {:error, term()}
  @callback new_icon(app :: atom(), path :: String.t()) :: {:ok, term()} | {:error, term()}
  @callback new_icon_from(term()) :: {:ok, term()} | {:error, term()}
  @callback default_icon() :: {:ok, term()}
  @callback media_destroy(term()) :: :ok
  @callback object_type(term()) :: atom()

  def load_image(app, path), do: impl().load_image(app, path)
  def new_icon(app, path), do: impl().new_icon(app, path)
  def new_icon_from(image), do: impl().new_icon_from(image)
  def default_icon, do: impl().default_icon()
  def destroy(image), do: impl().media_destroy(image)
  def object_type(image), do: impl().object_type(image)

  defp impl, do: Desktop.Platform.backend()
end
