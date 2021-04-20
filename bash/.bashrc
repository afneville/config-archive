#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

#set -o vi

#This is a nice prompt variable, but it is too long :(
#PS1='\[\e[33m\]\A \[\e[01;36m\]\u \[\e[00m\]at \[\e[01;32m\]\h \[\e[00m\]in \[\e[01;34m\]\w \[\e[33m\]\$: \[\e[00m\]'
PS1='\[\e[34m\]\w\[\e[36m\] $: \[\e[00m\]'

# General:
alias grep='grep --color=auto'
alias config_monitor='xrandr --output eDP1 --mode 1920x1080 --auto --output HDMI1 --mode 1920x1080 --right-of eDP1 --primary  --auto'
alias ls='/usr/bin/exa -1a --group-directories-first'
alias lsl='/usr/bin/exa -lag --group-directories-first'
alias tree='/usr/bin/exa -T'
alias ..='cd ..'
alias ...='cd ../..'
alias locate='updatedb && locate'
alias volume='alsamixer'
alias nm='nm-connection-editor'
alias off="doas poweroff"
alias restart="doas reboot now"


alias hello='echo hi'

# I have trouble with "clear"
alias cleaer="clear"
alias cler="clear"
alias claer="clear"

# Safe Commands:
#alias rm="rm -i"
#alias mv="mv -i"
#alias vim="nvim"


# Directories:
alias web="cd /srv/http/"
alias code="cd ~/code/"
alias config="cd ~/code/dotfiles"
alias dwm="cd ~/code/suckless_tools/dwm"
alias test="cd /sandbox/"
alias nea="cd ~/code/nea"
# shopt -s autocd

alias g="git"
alias p="python3"
alias s="systemctl"
alias v="nvim"

alias clone="git clone"
alias push="git push"
alias pull="git pull"
alias add="git add"
alias commit="git commit -m"
alias gpom="git add . && git commit -m \"automated backup\" && git push origin main"

alias status="systemctl status"
alias start="systemctl start"
alias stop="systemctl stop"
alias restart="systemctl restart"

export PYTHONPATH=/home/alex/python/packages/
export PATH=$PATH:$HOME/.local/bin:$HOME/code/scripts:$HOME/.emacs.d/bin
export VISUAL=nvim
export EDITOR=nvim

umask 0022 
