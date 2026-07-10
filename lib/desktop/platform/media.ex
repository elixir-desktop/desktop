defmodule Desktop.Platform.Media do
  @moduledoc false

  alias Desktop.Platform.Helpers

  @callback load_image(app :: atom(), path :: String.t()) :: {:ok, term()} | {:error, term()}
  @callback new_icon(app :: atom(), path :: String.t()) :: {:ok, term()} | {:error, term()}
  @callback new_icon_from(term()) :: {:ok, term()} | {:error, term()}
  @callback default_icon() :: {:ok, term()}
  @callback media_destroy(term()) :: :ok
  @callback object_type(term()) :: atom()

  def load_image(app, path), do: Helpers.with_wx_env(fn -> impl().load_image(app, path) end)
  def new_icon(app, path), do: Helpers.with_wx_env(fn -> impl().new_icon(app, path) end)

  def new_icon_from(image),
    do: Helpers.with_wx_env(fn -> impl().new_icon_from(image) end)

  def default_icon, do: Helpers.with_wx_env(fn -> impl().default_icon() end)
  def destroy(image), do: Helpers.with_wx_env(fn -> impl().media_destroy(image) end)
  def object_type(image), do: Helpers.with_wx_env(fn -> impl().object_type(image) end)

  defp impl, do: Desktop.Platform.backend()
end
