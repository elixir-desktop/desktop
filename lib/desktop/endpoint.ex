defmodule Desktop.Endpoint do
  @doc false
  defmacro __using__(opts) do
    {scheme, opts} = Keyword.pop(opts, :desktop_scheme, :http)

    quote do
      use Phoenix.Endpoint, unquote(opts)
      defoverridable url: 0

      def url do
        url = super()
        scheme = unquote(scheme)

        case Keyword.get(config(scheme), :port, 0) do
          0 -> String.replace(url, ":0", ":#{get_dynamic_port(scheme)}")
          _port -> url
        end
      end

      def get_dynamic_port(scheme) do
        {:ok, {_ip, port}} = server_info(scheme)
        port
      end
    end
  end
end
