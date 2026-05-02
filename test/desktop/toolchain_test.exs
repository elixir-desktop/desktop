defmodule Desktop.ToolchainTest do
  use ExUnit.Case, async: true

  alias Desktop.Toolchain

  describe "verify/2" do
    test "ok when erlang major matches and elixir semver matches" do
      req = %{erlang: "26.2.5.5 system", elixir: "1.19.1-otp-26"}

      assert Toolchain.verify(req,
               otp_release: 26,
               elixir_version: "1.19.1"
             ) == :ok
    end

    test "error when OTP major mismatches" do
      req = %{erlang: "26.2.5.5 system", elixir: "1.19.1-otp-26"}

      assert {:error, msg} =
               Toolchain.verify(req,
                 otp_release: 25,
                 elixir_version: "1.19.1"
               )

      assert msg =~ "OTP"
      assert msg =~ "25"
      assert msg =~ "26"
    end

    test "error when Elixir semver mismatches" do
      req = %{erlang: "26.2.5.5 system", elixir: "1.19.1-otp-26"}

      assert {:error, msg} =
               Toolchain.verify(req,
                 otp_release: 26,
                 elixir_version: "1.18.0"
               )

      assert msg =~ "Elixir"
      assert msg =~ "1.18.0"
      assert msg =~ "1.19.1"
    end

    test "ok when only erlang line present" do
      req = %{erlang: "24.0.1"}

      assert Toolchain.verify(req, otp_release: 24, elixir_version: "9.9.9") == :ok
    end

    test "ok when only elixir line present" do
      req = %{elixir: "1.12.0"}

      assert Toolchain.verify(req, otp_release: 99, elixir_version: "1.12.0") == :ok
    end

    test "empty requirements always ok" do
      assert Toolchain.verify(%{}, otp_release: 1, elixir_version: "0.1.0") == :ok
    end
  end

  describe "verify_file/2" do
    test "reads and verifies temp file" do
      path =
        Path.join(
          System.tmp_dir!(),
          "desktop-tool-versions-test-#{:erlang.unique_integer([:positive])}"
        )

      content = """
      erlang 26.0.1
      elixir 1.14.0
      """

      :ok = File.write(path, content)

      try do
        assert Toolchain.verify_file(path,
                 otp_release: 26,
                 elixir_version: "1.14.0"
               ) == :ok

        assert {:error, _} =
                 Toolchain.verify_file(path,
                   otp_release: 25,
                   elixir_version: "1.14.0"
                 )
      after
        File.rm(path)
      end
    end

    test "missing file returns error" do
      path =
        Path.join(
          System.tmp_dir!(),
          "nonexistent-tool-versions-#{:erlang.unique_integer([:positive])}"
        )

      assert {:error, msg} = Toolchain.verify_file(path)
      assert msg =~ "Could not read"
    end
  end

  describe "helpers" do
    test "erlang_major/1" do
      assert Toolchain.erlang_major("26.2.5.5 system") == 26
      assert Toolchain.erlang_major("24.0.1") == 24
    end

    test "elixir_base_version/1" do
      assert Toolchain.elixir_base_version("1.19.1-otp-26") == "1.19.1"
      assert Toolchain.elixir_base_version("1.12.0") == "1.12.0"
    end
  end
end
