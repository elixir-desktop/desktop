defmodule Desktop.Platform.Backend do
  @moduledoc false

  @type capabilities :: Desktop.Platform.capabilities()

  @callback capabilities() :: capabilities()
end
