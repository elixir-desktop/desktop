defmodule Desktop do
  @moduledoc """
  This is the documentation for the Desktop project.

  By default, Desktop applications depend on the following packages:

    * [Phoenix](https://hexdocs.pm/phoenix) - the Phoenix web framework

    * [Phoenix LiveView](https://hexdocs.pm/phoenix_live_view) - real-time
      user experience

    * [Sqlite3 Ecto](https://github.com/elixir-sqlite/ecto_sqlite3) - local
      database

  To get started, see our [guides](introduction.html).
  """
  use Application

  @doc false
  def start(:normal, []) do
    child = %{
      id: Desktop.Env,
      start: {Desktop.Env, :start_link, []}
    }

    Supervisor.start_link([child], strategy: :one_for_one, name: Desktop.Sup)
  end

  @doc false
  @spec start_link() :: :ignore | {:error, any} | {:ok, pid}
  def start_link() do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
  end

  @doc """
    Call on application to use the system locale by default for
    translations. The function will scan your gettext backend for available
    languages and compare to the system language to select the best match.

    ```
      Desktop.identify_default_locale(MyApp.Gettext)
    ```
  """
  def identify_default_locale(backend) do
    :wx.set_env(Desktop.Env.wx_env())
    locale = :wxLocale.new(:wxLocale.getSystemLanguage())
    # this is in the form "xx_XX"
    language_code = :wxLocale.getCanonicalName(locale) |> List.to_string() |> String.downcase()

    # All locales with translations:
    known_locales = Gettext.known_locales(backend)

    # Looking for a best fit
    # Preferring a full match 'xx_xx' == 'yy_yy'
    best_match = Enum.find(known_locales, fn l -> String.downcase(l) == language_code end)

    if best_match != nil do
      put_default_locale(best_match)
    else
      # Looking for a prefix match 'xx' == 'yy'
      prefix = binary_part(language_code, 0, 2)

      prefix_match =
        Enum.find(known_locales, fn l -> String.starts_with?(String.downcase(l), prefix) end)

      if prefix_match != nil do
        put_default_locale(prefix_match)
      else
        # we're giving up, not updating the default locale
      end
    end
  end

  @doc """
    Allows setting the default locale that will be used for translations in this
    Desktop application.
  """
  def put_default_locale(locale) when is_binary(locale) do
    # :persistent_term.put(@key, locale)
    Application.put_env(:gettext, :default_locale, locale)
  end
end
