defmodule Desktop.ToolchainTest do
  use ExUnit.Case, async: true

  @moduletag :tmp_dir

  setup context do
    root = Path.join(context.tmp_dir, "proj")
    File.mkdir_p!(root)
    Map.put(context, :root, root)
  end

  test "returns no_tool_versions when .tool-versions is missing", %{root: root} do
    assert Desktop.Toolchain.verify(root) == {:ok, :no_tool_versions}
  end

  test "verified when versions match", %{root: root} do
    File.write!(Path.join(root, ".tool-versions"), "erlang 26.2.5.5\nelixir 1.19.1-otp-26\n")

    assert Desktop.Toolchain.verify(root,
             otp_release: "26",
             elixir_version: "1.19.1"
           ) == {:ok, :verified}
  end

  test "error when OTP major mismatches", %{root: root} do
    File.write!(Path.join(root, ".tool-versions"), "erlang 26.0.1\n")

    assert {:error, [msg]} =
             Desktop.Toolchain.verify(root, otp_release: "25", elixir_version: "1.19.1")

    assert msg =~ "OTP"
  end

  test "error when Elixir semver mismatches", %{root: root} do
    File.write!(Path.join(root, ".tool-versions"), "elixir 1.19.1-otp-26\n")

    assert {:error, [msg]} =
             Desktop.Toolchain.verify(root, otp_release: "26", elixir_version: "1.18.0")

    assert msg =~ "Elixir mismatch"
  end

  test "error when .tool-versions cannot be read", %{root: root} do
    File.mkdir!(Path.join(root, ".tool-versions"))

    assert {:error, [msg]} = Desktop.Toolchain.verify(root)
    assert msg =~ "Could not read .tool-versions"
  end
end
