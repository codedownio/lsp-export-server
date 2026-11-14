# lsp-export-server

A language server that renders documents from the buffers a client sends it, and streams the
result back as xdelta3 diffs. The codedown runner uses it for live exports: an edit reaches the
server as `didChange` and a new render comes back without a trip through the disk.

The server knows nothing about document formats. The packaging names the program that renders:

```
lsp-export-server serve --render latex-render --settle latex-render --locate latex-locate -o engine=pdflatex   # the LSP, on stdio
lsp-export-server render --render latex-render input.tex output.pdf                      # once, from disk
```

Each edit gets a quick render with the `--render` program once edits pause for the debounce
period. With a `--settle` program, a settle pass follows once edits stay quiet for longer (a full
latexmk build, say); only the quick pass reports `rendering`, and an output identical to the last
one is not sent.

## The render program

It runs in the document's directory, so relative paths resolve from the document's real tree,
with these variables:

| Variable | |
|---|---|
| `RENDER_PHASE` | `quick` or `settle` |
| `RENDER_DOCUMENT` | the real document's absolute path |
| `RENDER_ENTRY` | the document's current buffer, written to `RENDER_SRC` |
| `RENDER_OVERLAY` | the other open buffers, at their paths relative to the document's directory; a file here is newer than its disk copy |
| `RENDER_OUT` | scratch space kept across renders, for aux files and the like |
| `RENDER_WORK` | the task's work directory holding `src/`, `overlay/` and `out/` |
| `RENDER_JOB`, `RENDER_FORMAT` | the document's base name and the output extension |
| `RENDER_OUTPUT` | where to write the result: `RENDER_OUT/RENDER_JOB.RENDER_FORMAT` |
| `RENDER_DEPS` | optional: write the files read, one path per line, absolute or relative to the directory |
| `RENDER_EPOCH` | a Unix time to use for timestamps in the output (`SOURCE_DATE_EPOCH`), fixed per task so unchanged input gives unchanged bytes |
| `RENDER_OPT_<key>` | each `-o key=value` from the command line and each option in the start request |

Its stdout and stderr are streamed to the client as the log. A non-zero exit reports an error with
that exit code. Paths listed in `RENDER_DEPS` under `RENDER_SRC` or `RENDER_OVERLAY` are mapped
back to the documents they stand for; the client is told to watch the ones it does not hold open,
and a `rebuild` renders again only when one of them changed.

## The locate program

Optional. It maps between source positions and places in the last output, as SyncTeX does for
LaTeX. It runs in the document's directory with `RENDER_DOCUMENT`, `RENDER_JOB`, `RENDER_FORMAT`,
`RENDER_OUTPUT`, `RENDER_WORK` and `RENDER_OUT` as for the render program, and:

| Variable | |
|---|---|
| `RENDER_QUERY` | `forward` or `inverse` |
| `RENDER_SOURCE`, `RENDER_LINE`, `RENDER_COLUMN` | forward: the file as the render program saw it (the copy under `RENDER_WORK` for a buffer), and a one-based line and column |
| `RENDER_PAGE`, `RENDER_X`, `RENDER_Y` | inverse: a one-based page, and a point on it in PDF points from its top-left corner |
| `RENDER_RESULT` | where to write the answer, one line: `page left top width height` for forward, in the same units; `line column file` for inverse, one-based. Nothing when there is no match |

After each render that follows an edit, the server locates the edit and sends the box with the
output as `position`. The `locate` and `sourceAt` commands answer the same queries for the client,
from the last render. Files under `RENDER_WORK` in an inverse answer are mapped back to the
documents they stand for, as with `RENDER_DEPS`.

## Protocol

Commands (`workspace/executeCommand`): `codedown.render.start`, `codedown.render.stop`,
`codedown.render.rebuild`, `codedown.render.setPaused`, `codedown.render.resync`,
`codedown.render.locate`, `codedown.render.sourceAt`. Notifications:
`codedown/render/status`, `codedown/render/output`, `codedown/render/log`. See `src/Render/Protocol.hs`.
Statuses carry the pass (`phase`) and what triggered it (`reason`), and `──` marker lines in the
log say when each pass started, why, how long it took and what was sent.

`cbits/` is xdelta3, Apache-2.0 (see `cbits/LICENSE`).

## Developing

`nix develop -c stack build --test`. The tests drive the built executable through lsp-test with
shell scripts as render programs.
