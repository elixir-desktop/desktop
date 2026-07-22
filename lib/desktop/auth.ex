defmodule Desktop.Auth do
  @moduledoc """
    Auth provider that ensures that only the wanted WebView can open an interact
    with the application. A token is generated and compared to ensure no other application
    can connect to the local webserver.
  """

  import Plug.Conn
  alias Desktop.OS
  @behaviour Plug

  @table __MODULE__
  @key {__MODULE__, :key}

  defp key() do
    # key should stay the same during application run,
    # but be different on each instance
    case :persistent_term.get(@key, nil) do
      nil -> init_key()
      key -> key
    end
  end

  # `persistent_term` put is not atomic with get: under concurrent first access
  # (e.g. Window.prepare_url/1 and this plug) two processes can mint different
  # keys, leave the webview with a stale `?k=`, and serve a blank "Unauthorized".
  # ETS insert_new elects a single winner; persistent_term remains the fast path.
  defp init_key() do
    table = table!()

    case :ets.lookup(table, :key) do
      [{:key, key}] ->
        store_key(key)

      [] ->
        key = :crypto.strong_rand_bytes(32)

        if :ets.insert_new(table, {:key, key}) do
          store_key(key)
        else
          [{:key, key}] = :ets.lookup(table, :key)
          store_key(key)
        end
    end
  end

  defp store_key(key) do
    :persistent_term.put(@key, key)
    key
  end

  defp table!() do
    case :ets.whereis(@table) do
      :undefined ->
        try do
          :ets.new(@table, [:named_table, :public, :set, read_concurrency: true])
        rescue
          ArgumentError ->
            # Concurrent create — the other process owns the table now.
            table!()
        end

      tid ->
        tid
    end
  end

  def set_key(key) do
    decoded = Base.decode32!(key)
    :ets.insert(table!(), {:key, decoded})
    :persistent_term.put(@key, decoded)
  end

  def login_key() do
    Base.encode32(key(), padding: false)
  end

  def init([]), do: []

  def call(conn, _options \\ []) do
    conn = fetch_session(conn)

    case get_session(conn, :user) do
      nil -> require_auth(conn)
      true -> conn
    end
  end

  defp require_auth(conn) do
    conn = fetch_query_params(conn)

    if OS.mobile?() or Plug.Crypto.secure_compare(login_key(), conn.query_params["k"] || "") do
      put_session(conn, :user, true)
    else
      conn
      |> resp(401, "Unauthorized")
      |> halt()
    end
  end
end
