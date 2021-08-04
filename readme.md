# About

This is a Haskell implementation of an [mpv] client using its [JSON IPC API](ipc), built with [polysemy].

⚠ Under Construction ⚠

# Conceptual Example

```haskell
prog =
  interpretMpvNative do
    withMpv do
      Mpv.command (Command.Load [relfile|vid.mkv|])
      Mpv.command (Command.Seek 50 (SeekFlags Absolute SeekFlags.Percent Exact))
      Mpv.command Command.Stop
```

[mpv]: https://mpv.io
[ipc]: https://mpv.io/manual/master/#json-ipc
[polysemy]: https://hackage.haskell.org/package/polysemy
