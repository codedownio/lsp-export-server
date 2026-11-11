# Changelog

## 0.5.0.0

- The server no longer knows any document format. `--render EXE` (and optionally `--settle EXE`) name the programs that render; they run in the document's directory with the task's layout and options in `RENDER_*` variables, write `RENDER_OUTPUT`, and may list what they read in `RENDER_DEPS`. The `latex` and `pandoc` backends and `--backend` are gone; their logic moves to the packaging.
- No settle pass without a `--settle` program.

## 0.4.1.2

- Release artifacts for aarch64-linux, x86_64-darwin and aarch64-darwin alongside x86_64-linux.

## 0.4.1.1

- A rebuild request before any dependency is known no longer restarts the first render.

## 0.4.1.0

- A `pandoc` backend: pandoc converts the buffer, and the LaTeX-based writers go through the LaTeX backend's quick and settle passes.
- Tasks and one-shot renders see the output extension as the `format` option.

## 0.4.0.0

- A rebuild request only renders when some on-disk dependency's content changed; the reason names the files.
- The settle pass is silent in the log unless it fails or changes the output.

## 0.3.0.0

- Statuses carry the pass (`phase`: quick or settle) and the `reason` the render ran.
- The settle pass no longer reports `rendering`.
- Marker lines in the log say when each pass started, why, how long it took and what was sent.

## 0.2.0.0

- Renamed from render-server.
- Renders stamp a fixed per-task SOURCE_DATE_EPOCH, and an output identical to the last one is not published.
- Reported dependencies leave out the buffers the client sends.
- Logs the debounce period when a task starts.

## 0.1.0.0

- First release: the LaTeX backend.
