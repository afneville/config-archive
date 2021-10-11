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

# uncomment for space before and after output.
# preexec() {
# 
#     echo ""
# 
# }

# precmd() {
#     if [ -z "$OPTIONAL_NEW_LINE" ]; then
#         OPTIONAL_NEW_LINE=1
#     elif [ "$OPTIONAL_NEW_LINE" -eq 1 ]; then
#         echo "\n"
#     fi
# }

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

PROMPT="%B%{$fg[blue]%}[%{$fg[yellow]%}%n%{$fg[cyan]%}@%{$fg[green]%}%m%{$fg[blue]%}] %{$fg[blue]%}[%{$fg[cyan]%}%~%{$fg[blue]%}]\$vcs_info_msg_0_ >> "


# 3 line prompt
# PROMPT=$'\n %{$fg[blue]%}┌[%{$fg_bold[cyan]%}%n%{$reset_color%}%{$fg[blue]%}@%{$fg_bold[cyan]%}%m%{$reset_color%}%{$fg[blue]%}] [%{$fg_bold[cyan]%}/dev/%y%{$reset_color%}%{$fg[blue]%}]\n |\n └[%{$fg_bold[cyan]%}%~%{$reset_color%}%{$fg[blue]%}]>%{$reset_color%} '

# optinal git info
# PROMPT+="\$vcs_info_msg_0_ "
