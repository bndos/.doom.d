#!/usr/bin/env sh

# go
go install github.com/go-delve/delve/cmd/dlv@latest
# python
uv tool install debugpy
# node
mkdir -p ~/.config/emacs/debug-adapters
wget -O ~/.config/emacs/debug-adapters/js-debug-dap-v1.97.1.tar.gz https://github.com/microsoft/vscode-js-debug/releases/download/v1.97.1/js-debug-dap-v1.97.1.tar.gz
tar -xvzf ~/.config/emacs/debug-adapters/js-debug-dap-v1.97.1.tar.gz -C ~/.config/emacs/debug-adapters/
