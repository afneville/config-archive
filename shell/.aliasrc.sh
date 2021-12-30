# General:
alias grep='grep --color=auto'
alias config_monitor='xrandr --output eDP1 --mode 1920x1080 --auto --output HDMI1 --mode 1920x1080 --right-of eDP1 --primary  --auto'
alias ls='/usr/bin/exa -a --group-directories-first'
alias lsl='/usr/bin/exa -lag --group-directories-first --icons'
alias sudo="doas"
alias tree='exa --tree --icons'

alias locate='updatedb && locate'
alias volume='alsamixer'
alias nm='nm-connection-editor'
alias off="poweroff"
alias restart="reboot now"
alias server="ssh alex@192.168.1.19"
alias aliases="nvim ~/.aliasrc.sh"
alias x="startx"
alias c="unset OPTIONAL_NEW_LINE && clear"
alias clear="unset OPTIONAL_NEW_LINE && clear"
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
alias todo="nvim ~/TODO"
alias pls="doas"

# Directories:
alias ..='cd ..'
alias ...='cd ../..'
alias web="cd /srv/http/"
alias code="cd ~/code/"
alias config="cd ~/code/dotfiles"
alias sand="cd ~/code/sandbox"
alias nea="cd ~/code/nea && source env/bin/activate"
alias opt="cd /opt"

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
alias gbak="git add . && git commit -m \"automated backup\" && git push origin main"
alias gpom="git push origin main"
alias gs="git status"
alias ga="git add"
alias gc="git commit"

# Systemctl
alias sstat="systemctl status"
alias sstart="systemctl start"
alias sstop="systemctl stop"
alias srestart="systemctl restart"
alias senable="systemctl enable"

# Gentoo
alias accept="doas nvim /etc/portage/package.accept_keywords"
alias package.use="doas nvim /etc/portage/package.use"
alias make.conf="doas nvim /etc/portage/make.conf"
