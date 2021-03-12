#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

#set -o vi

#This is a nice prompt variable, but it is too long :(
#PS1='\[\e[33m\]\A \[\e[01;36m\]\u \[\e[00m\]at \[\e[01;32m\]\h \[\e[00m\]in \[\e[01;34m\]\w \[\e[33m\]\$: \[\e[00m\]'
PS1='\[\e[34m\]\w\[\e[36m\] $: \[\e[00m\]'

alias grep='grep --color=auto'
alias config_monitor='xrandr --output eDP1 --mode 1920x1080 --auto --output HDMI1 --mode 1920x1080 --right-of eDP1 --primary  --auto'
alias exa='exa -lag'
alias ..='cd ..'
alias ...='cd ../..'
alias locate='updatedb && locate'
alias volume='alsamixer'
alias nm='nm-connection-editor'
#alias python='python3'
#alias install="sudo pacman -S"
#alias update="sudo pacman -Syu"
#alias rm="rm -i"
#alias mv="mv -i"
#alias vim="nvim"
#alias sudo="doas"
alias off="doas poweroff"
alias restart="doas reboot now"

alias web="cd /srv/http/"
alias vc="cd ~/vc_projects"
# shopt -s autocd

alias clone="git clone"
alias push="git push"
alias pull="git pull"
alias add="git add"
alias commit="git commit -m"
alias gpom="git add . && git commit -m \"automated backup\" && git push origin master"
alias gpompc="git add . && git commit -m \"automated backup\" && git push origin main"

alias status="systemctl status"
alias start="systemctl start"
alias stop="systemctl stop"
alias restart="systemctl restart"

export PYTHONPATH=/home/alex/python/packages/
export PATH=$PATH:$HOME/.emacs.d/bin:/opt/lampp:$HOME/.local/bin:$HOME/.config/composer/vendor/bin:$HOME/vc_projects/scripts
export VISUAL=nvim
export EDITOR=nvim

umask 0022 
