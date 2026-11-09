# lsp-export-server

A language server that renders documents from the buffers a client sends it, and streams the
result back as xdelta3 diffs. The codedown runner uses it for live exports: an edit reaches the
server as `didChange` and a new render comes back without a trip through the disk.

```
lsp-export-server serve --backend latex -o engine=pdflatex     # the LSP, on stdio
lsp-export-server render --backend latex input.tex output.pdf  # once, from disk
```

Commands (`workspace/executeCommand`): `codedown.render.start`, `codedown.render.stop`,
`codedown.render.rebuild`, `codedown.render.setPaused`, `codedown.render.resync`. Notifications:
`codedown/render/status`, `codedown/render/output`, `codedown/render/log`. See `src/Render/Protocol.hs`.

`cbits/` is xdelta3, Apache-2.0 (see `cbits/LICENSE`).
