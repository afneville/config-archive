typeset -U PATH path
path=("$HOME/.local/bin" "$HOME/code/scripts" "$HOME/.emacs.d/bin")

export PATH=$PATH:$HOME/code/scripts/rofi
export PATH=$PATH:$HOME/code/scripts/dmenu
# export PATH=$PATH:$HOME/code/scripts/wm

export TERM="xterm-256color"
export QT_QPA_PLATFORMTHEME=qt5ct
export VISUAL=/usr/bin/nvim
export EDITOR=/usr/bin/nvim

export MANPAGER="sh -c 'col -bx | bat -l man -p'"
# export MANPAGER="nvim -c 'set ft=man' -"

export ZDOTDIR="$HOME/.config/zsh"
