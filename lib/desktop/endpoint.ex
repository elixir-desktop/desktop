defmodule Desktop.Endpoint do
  @doc false
  defmacro __using__(opts) do
    quote do
      use Phoenix.Endpoint, unquote(opts)
      defoverridable url: 0

      def url do
        url =
          Phoenix.Config.cache(
            __MODULE__,
            :__phoenix_url__,
            &Phoenix.Endpoint.Supervisor.url/1
          )

        endpoint = Module.safe_concat(__MODULE__, HTTP)
        String.replace(url, ":0", ":#{:ranch.get_port(endpoint)}")
      end
    end
  end
end
