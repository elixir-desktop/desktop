import Config

config :phoenix, :json_library, Jason

# Set at compile time from MIX_TARGET (no Mix at runtime in releases).
config :desktop, :mobile_target, Mix.target() in [:android, :ios]
