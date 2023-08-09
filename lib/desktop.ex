defmodule Desktop do
  @moduledoc """
  This is the documentation for the Desktop project.

  By default, Desktop applications depend on the following packages:

    * [Phoenix](https://hexdocs.pm/phoenix) - the Phoenix web framework

    * [Phoenix LiveView](https://hexdocs.pm/phoenix_live_view) - real-time
      user experience

    * [Sqlite3 Ecto](https://github.com/elixir-sqlite/ecto_sqlite3) - local
      database

  To get started, see the [Getting Started Guide](./guides/getting_started.md).

  This library is still worked on heaviliy and APIs are going to change in v2. If you are curious
  to play around or contribute it's to best clone the current sample app at:
  https://github.com/elixir-desktop/desktop-example-app and start modifying it to your wishes.

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
    # this is in the form "xx_XX"
    language_code = language_code()

    # All locales with translations:
    known_locales = Gettext.known_locales(backend)

    # Looking for a best fit
    # Preferring a full match 'xx_xx' == 'yy_yy'
    best_match = Enum.find(known_locales, fn l -> String.downcase(l) == language_code end)

    cond do
      best_match != nil ->
        put_default_locale(best_match)

      # We have seen windows return an empty string for language_code
      byte_size(language_code) >= 2 ->
        # Looking for a prefix match 'xx' == 'yy'
        prefix = binary_part(language_code, 0, 2)

        prefix_match =
          Enum.find(known_locales, fn l -> String.starts_with?(String.downcase(l), prefix) end)

        if prefix_match != nil do
          put_default_locale(prefix_match)
        end

      true ->
        # we're giving up, not updating the default locale
        nil
    end
  end

  def language_code() do
    with MacOS <- Desktop.OS.type(),
         {locale, 0} <- System.cmd("defaults", ~w(read -g AppleLocale)),
         [code] <- Regex.run(~r/[a-z]+_[A-Z]+/, locale) do
      # On MacOS getSystemLanguage() is calculated based on the the number
      # format (so mostly English) and not based on the actual used language
      # https://trac.wxwidgets.org/ticket/11594

      # So we are using this workaround instead to get the current locale as "xx_XX"
      # https://stackoverflow.com/questions/661935/how-to-detect-current-locale-in-mac-os-x-from-the-shell
      code
    else
      _ ->
        :wx.set_env(Desktop.Env.wx_env())
        locale = :wxLocale.new(:wxLocale.getSystemLanguage())
        # This is in the form "xx_XX"
        # we have seen windows returns an empty string though...
        :wxLocale.getCanonicalName(locale) |> List.to_string() |> String.downcase()
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

  @live_view_version Application.spec(:phoenix_live_view, :vsn) |> List.to_string()
  def live_view_version(), do: @live_view_version
end
