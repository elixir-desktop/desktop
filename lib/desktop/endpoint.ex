defmodule Desktop.Endpoint do
  @doc false
  defmacro __using__(opts) do
    quote do
      use Phoenix.Endpoint, unquote(opts)
      defoverridable url: 0

      def url do
        # Checking for pre LiveView 0.18.0
        url =
          if Version.compare(Desktop.live_view_version(), "0.18.0") == :lt do
            Phoenix.Config.cache(
              __MODULE__,
              :__phoenix_url__,
              fn a -> apply(Phoenix.Endpoint.Supervisor, :url, [a]) end
            )
          else
            :persistent_term.get({Phoenix.Endpoint, __MODULE__}, nil).url
          end

        endpoint = Module.safe_concat(__MODULE__, HTTP)
        String.replace(url, ":0", ":#{:ranch.get_port(endpoint)}")
      end
    end
  end
end
