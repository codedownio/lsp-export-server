# Changelog

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
