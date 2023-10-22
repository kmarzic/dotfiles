#!/bin/bash

echo "# tmux new -s root"
echo "# tmux new -s work"
echo "# tmux new -s ssh"
echo "# tmux new -s sp"
echo "# tmux new -s home"

[[ ! -d ~/.tmux ]] && echo "# mkdir ~/.tmux"
[[ ! -d ~/.tmux ]] && mkdir ~/.tmux

[[ ! -d ~/.tmux/plugins ]] && echo "# mkdir ~/.tmux/plugins"
[[ ! -d ~/.tmux/plugins ]] && mkdir ~/.tmux/plugins

[[ ! -d ~/.tmux/plugins/tpm ]] && echo "# git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm"
[[ ! -d ~/.tmux/plugins/tpm ]] && git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm

[[ -d ~/.tmux/plugins/tpm ]] && echo "# cd ~/.tmux/plugins/tpm && git pull"
[[ -d ~/.tmux/plugins/tpm ]] && cd ~/.tmux/plugins/tpm && git pull

#### END
