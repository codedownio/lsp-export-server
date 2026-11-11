# Changelog

## 0.1.0.0

- First release. A language server that renders a document from the buffers the client sends and
  streams the result back as xdelta3 diffs: `codedown.render.start/stop/rebuild/setPaused/resync`
  with `codedown/render/status`, `output` and `log` notifications.
- `--render EXE`, and optionally `--settle EXE`, name the programs that render. They run in the
  document's directory with the task's layout and options in `RENDER_*` variables, write
  `RENDER_OUTPUT`, and may list what they read in `RENDER_DEPS`.
- An edit gets a quick render once edits pause for the debounce period, and the settle pass follows
  once they stay quiet for longer. An output identical to the last one is not sent, and renders
  stamp a fixed per-task `SOURCE_DATE_EPOCH` so unchanged input gives unchanged bytes.
- A rebuild request renders only when some on-disk dependency's content changed.
