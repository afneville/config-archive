#!/bin/sh

## autoload vcs and colors
autoload -Uz vcs_info
autoload -U colors && colors

# enable only git 
zstyle ':vcs_info:*' enable git 

# setup a hook that runs before every ptompt. 
precmd_vcs_info() { vcs_info }
precmd_functions+=( precmd_vcs_info )
setopt prompt_subst

preexec() {

    echo ""

}

zstyle ':vcs_info:git*+set-message:*' hooks git-untracked
+vi-git-untracked(){
    if [[ $(git rev-parse --is-inside-work-tree 2> /dev/null) == 'true' ]] && \
        git status --porcelain | grep '??' &> /dev/null ; then

        # untracked files in pwd
        #[[ -n $(git ls-files --others --exclude-standard) ]] ; then

        # untracked files in whole repo
        hook_com[staged]+='!'
    fi
}

zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:git:*' formats " %{$fg[blue]%}(%{$fg[red]%}%m%u%c%{$fg[yellow]%}%{$fg[magenta]%} %b%{$fg[blue]%})"

# PS1='%B%F{cyan}%0d%f%b %F{yellow}>>%f '
# PROMPT="%B%{$fg[blue]%}[%{$fg[yellow]%}%n%{$fg[red]%}@%{$fg[cyan]%}%m%{$fg[blue]%}] %(?:%{$fg_bold[green]%}➜ :%{$fg_bold[red]%}➜ )%{$fg[cyan]%}%c%{$reset_color%}"
# PROMPT=$'\n %{$fg[blue]%}┌[%{$fg_bold[white]%}%n%{$reset_color%}%{$fg[blue]%}@%{$fg_bold[white]%}%m%{$reset_color%}%{$fg[blue]%}] [%{$fg_bold[white]%}/dev/%y%{$reset_color%}%{$fg[blue]%}]\n |\n └[%{$fg_bold[white]%}%~%{$reset_color%}%{$fg[blue]%}]>%{$reset_color%} '
PROMPT=$'\n %{$fg[blue]%}┌[%{$fg_bold[cyan]%}%n%{$reset_color%}%{$fg[blue]%}@%{$fg_bold[cyan]%}%m%{$reset_color%}%{$fg[blue]%}] [%{$fg_bold[cyan]%}/dev/%y%{$reset_color%}%{$fg[blue]%}]\n |\n └[%{$fg_bold[cyan]%}%~%{$reset_color%}%{$fg[blue]%}]>%{$reset_color%} '
# PROMPT="%{$fg[green]%}%n@%m %~ %{$reset_color%}%#> "
# PROMPT+="\$vcs_info_msg_0_ "
