defmodule Desktop.EnvTest do
  use Desktop.Test.DesktopCase, async: false

  describe "browser backend" do
    test "T-ENV-03: wx_use_env with nil wx_env is no-op" do
      with_backend(:browser, fn ->
        restart_desktop!()
        assert :ok = Desktop.Env.wx_use_env()
      end)
    end
  end

  describe "subscriber payload normalization" do
    setup do
      previous = Application.get_env(:desktop, :backend, :auto)
      Application.put_env(:desktop, :backend, :browser)
      restart_desktop!()
      :ok = Desktop.Env.subscribe()

      on_exit(fn ->
        Application.put_env(:desktop, :backend, previous)
      end)

      :ok
    end

    test "notify_subscribers keeps binary open_url paths" do
      Desktop.Env.notify_subscribers({:open_url, ["ddrive://x"]})
      assert_receive {:open_url, [url]}, 1000
      assert url == "ddrive://x"
      assert is_binary(url)
    end

    test "notify_subscribers converts charlist open_url paths to binaries" do
      Desktop.Env.notify_subscribers({:open_url, [~c"ddrive://x"]})
      assert_receive {:open_url, ["ddrive://x"]}, 1000
    end

    test "notify_subscribers converts list of charlist open_file paths" do
      Desktop.Env.notify_subscribers({:open_file, [~c"/tmp/a", ~c"/tmp/b"]})
      assert_receive {:open_file, ["/tmp/a", "/tmp/b"]}, 1000
    end

    test "wx ingest with bare charlist second element becomes binary list" do
      send(Desktop.Env, {:open_url, ~c"ddrive://bare"})
      assert_receive {:open_url, ["ddrive://bare"]}, 1000
    end

    test "buffered OS events before any subscriber are normalized binaries" do
      Application.put_env(:desktop, :backend, :browser)
      restart_desktop!()

      # No subscribers yet — event is buffered and normalized on ingest.
      send(Desktop.Env, {:open_url, [~c"ddrive://buffered"]})
      Process.sleep(50)

      :ok = Desktop.Env.subscribe()
      assert_receive {:open_url, ["ddrive://buffered"]}, 1000
    end

    test "notify_subscribers leaves window_activated unchanged" do
      Desktop.Env.notify_subscribers({:desktop, :window_activated, :TestWindow})
      assert_receive {:desktop, :window_activated, :TestWindow}, 1000
    end

    test "notify_subscribers leaves new_file unchanged" do
      Desktop.Env.notify_subscribers({:new_file, []})
      assert_receive {:new_file, []}, 1000
    end
  end

  defp restart_desktop! do
    _ = Application.stop(:desktop)
    {:ok, _} = Application.ensure_all_started(:desktop)
    :ok
  end
end
