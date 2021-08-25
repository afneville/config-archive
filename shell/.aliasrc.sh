# General:
alias grep='grep --color=auto'
alias config_monitor='xrandr --output eDP1 --mode 1920x1080 --auto --output HDMI1 --mode 1920x1080 --right-of eDP1 --primary  --auto'
alias ls='/usr/bin/exa -a --group-directories-first --icons'
alias lsl='/usr/bin/exa -lag --group-directories-first --icons'

alias locate='updatedb && locate'
alias volume='alsamixer'
alias nm='nm-connection-editor'
alias off="poweroff"
alias restart="reboot now"
alias server="ssh alex@192.168.1.19"
alias aliases="nvim ~/.aliasrc.sh"
alias x="startx"
alias c="clear"
alias merge="xrdb -merge ~/.Xresources"

# tmux
alias tls="tmux ls"
alias tat="tmux attach -t"
alias tdat="tmux detach"
alias tnew="tmux new -s"
alias trens="tmux rename-session"
alias trenw="tmux rename-window"
alias tkill="tmux kill-session -t"
alias tend="tmux kill-server"

# Fun
alias hello="figlet hi"
alias say="figlet"
alias info="echo \"\" && pfetch"
alias connection="ping archlinux.org"

# Safe Commands:
#alias rm="rm -i"
#alias mv="mv -i"
#alias vim="nvim"

# Directories:
alias ..='cd ..'
alias ...='cd ../..'
alias web="cd /srv/http/"
alias code="cd ~/code/"
alias config="cd ~/code/dotfiles"
alias sand="cd ~/code/sandbox"
alias nea="cd ~/code/nea && source env/bin/activate"
alias opt="cd /opt"
# shopt -s autocd

# Utilities
alias g="git"
alias p="python3"
alias s="systemctl"
alias v="nvim"
alias top="htop"
alias t="htop"

# Git
alias clone="git clone"
alias push="git push"
alias pull="git pull"
alias add="git add"
alias commit="git commit -m"
alias gpom="git add . && git commit -m \"automated backup\" && git push origin main"

# Systemctl
alias status="systemctl status"
alias start="systemctl start"
alias stop="systemctl stop"
alias restart="systemctl restart"
