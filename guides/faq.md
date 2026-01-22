# Frequently Asked Questions (FAQ)

## How do I release and distribute my Desktop application?

### Creating an Installer

To create a distributable installer for your Desktop application, use the following command:

```bash
mix desktop.installer
```

This command will create platform-specific installers for Windows, macOS, and Linux that you can distribute to your users.

### About `mix release`

While `mix release` is a standard Elixir command for creating releases, it's not the recommended approach for Desktop applications. The `mix release` command creates a standalone Erlang release, but it doesn't include the platform-specific packaging and UI components needed for a Desktop application.

### Distribution

After running `mix desktop.installer`, you'll find the installer files in your project's build directory. These are the files you should distribute to your users:

- **Windows**: `.exe` installer file
- **macOS**: `.dmg` or `.app` bundle
- **Linux**: `.AppImage`, `.deb`, or `.rpm` package

Users can simply download and run these installers to install your application on their system.

### Getting Started

If you're new to Elixir Desktop:

1. Follow the [Getting your Environment Ready Guide](getting_started.md) to set up your development environment
2. Create your first app with the [Your first Desktop App Guide](your_first_desktop_app.md)
3. When ready to distribute, run `mix desktop.installer` to create platform-specific installers
