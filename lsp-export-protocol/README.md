# lsp-export-protocol

The `codedown/render` wire types, shared by `lsp-export-server` and its clients. Depends only on
`aeson`, `containers` and `text`, so a client needs no LSP dependency to speak the protocol.

A *task* is one document rendered to one format. The client picks its `taskId`; every message
carries it.

Field names are the record field with its prefix dropped, first letter lower-cased: `startTaskId`
is `taskId`. Enums drop the constructor prefix and lower-case: `StatusRendering` is `"rendering"`.

## Messages

The document arrives as ordinary `textDocument/didOpen` and `didChange`, so renders come from the
buffer rather than from disk. Commands are `workspace/executeCommand` requests; a rejected one
comes back as an `InvalidParams` error response. Renders report progress as custom notifications,
since they outlive the command that started them.

| client → server | arguments | result |
| --- | --- | --- |
| `codedown.render.start` | `[StartParams]` | `StartResult` |
| `codedown.render.stop` | `[taskId]` | `null` |
| `codedown.render.rebuild` | `[taskId, RebuildParams]` | `null` |
| `codedown.render.setPaused` | `[taskId, bool]` | `null` |
| `codedown.render.resync` | `[taskId]` | `null` |

| server → client | params |
| --- | --- |
| `codedown/render/status` | `StatusParams` |
| `codedown/render/output` | `OutputParams` |
| `codedown/render/log` | `LogParams` |

`StartResult` only echoes the `taskId` the client chose, so a client that tracks its own ids can
treat every command as fire-and-forget and watch for the error response.

`rebuild` says a file the last render read has changed; the server re-hashes the deps it knows and
renders only if one differs. `setPaused` suspends rendering without ending the task. `resync` makes
the next output a full one.

The notifications are externally tagged: the variant is the method, and the params carry no
discriminator of their own, so a client dispatches on the method and then parses the params.

## The cycle

1. An edit marks the task dirty. The worker sleeps `debounceMs`; another edit restarts the wait.
2. Quick pass: `status` `rendering` (`phase: "quick"`), a `log` with `clear: true`, then the render
   program runs with its output streaming as `log`.
3. `output` if the bytes changed, then `status` `ready` with `renderMs` and `timings`.
4. If the program has a settle pass and edits stay quiet for 1500ms, it runs the same way with
   `phase: "settle"`, its logs flushed only if it failed or changed the output.
5. An edit mid-render cancels it, with no `status` for that pass.

A failure sends `status` `errored` with the program's `exitCode`. Both passes of one edit report
`ready`, so a client tracking "is it rendering" should let `phase: "settle"` leave its state alone.

## Output

`OutputFile` has the base64 `bytes`, an `encoding`, and `decodedSize` — the size of the
reconstructed file, not of this message.

The first output is `full`; later ones are `xdelta3` patches against `baseRenderId`, the `renderId`
of the last output sent. So a client keeps the bytes it last reconstructed and:

- `baseRenderId` matches → apply, store, remember `renderId`.
- it doesn't, or the patch won't apply → send `resync`.
- bytes identical to the last render → nothing is sent at all.

If xdelta3 fails the server falls back to `full`.

`deps` lists files the render read from disk, minus the entry document and any buffer the client
sends over `didChange`; watch them and send `rebuild`. `documentVersion` is the entry buffer's LSP
version, absent when the server read from disk.
