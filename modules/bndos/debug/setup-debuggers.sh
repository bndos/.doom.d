#!/usr/bin/env sh

touch ~/.config/emacs/.local/cache/dape-breakpoints
# go
go install github.com/go-delve/delve/cmd/dlv@latest
# python
uv tool install debugpy
# node
mkdir -p ~/.config/emacs/.local/cache/debug-adapters
wget -O ~/.config/emacs/.local/cache/debug-adapters/js-debug-dap-v1.97.1.tar.gz https://github.com/microsoft/vscode-js-debug/releases/download/v1.97.1/js-debug-dap-v1.97.1.tar.gz
tar -xvzf ~/.config/emacs/.local/cache/debug-adapters/js-debug-dap-v1.97.1.tar.gz -C ~/.config/emacs/.local/cache/debug-adapters/
# bash
wget -O ~/.config/emacs/.local/cache/debug-adapters/bash-debug-0.3.9.vsix https://github.com/rogalmic/vscode-bash-debug/releases/download/untagged-438733f35feb8659d939/bash-debug-0.3.9.vsix
unzip ~/.config/emacs/.local/cache/debug-adapters/bash-debug-0.3.9.vsix -d ~/.config/emacs/.local/cache/debug-adapters/bash-debug
