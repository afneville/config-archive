HISTFILE=~/.cache/zsh_history
HISTSIZE=10000
SAVEHIST=10000

export PATH=$PATH:$HOME/.local/bin:$HOME/.emacs.d/bin:$HOME/.npm_global/bin
# vim mode
bindkey -v
export KEYTIMEOUT=1

# function zle-keymap-select () {
#     case $KEYMAP in
#         vicmd) echo -ne '\e[1 q';;      # block
#         viins|main) echo -ne '\e[5 q';; # beam
#     esac
# }
# zle -N zle-keymap-select
# zle-line-init() {
#     zle -K viins
#     echo -ne "\e[5 q"
# }
# zle -N zle-line-init
# echo -ne '\e[5 q'
# preexec() { echo -ne '\e[5 q' ;}


# features
setopt extendedglob nomatch menucomplete
setopt interactive_comments
zle_highlight=('paste:none')
unsetopt BEEP

autoload -U colors && colors
autoload -Uz compinit
compinit

zstyle :compinstall filename '/home/alex/.config/zsh/.zshrc'
zstyle ':completion:*' menu select
zstyle ':completion::complete:*' gain-privileges 1
_comp_options+=(globdots)


# source aliases
[ -f "$HOME/.aliasrc.sh" ] && source "$HOME/.aliasrc.sh"

# custom "plugin manager"
[ -f "$ZDOTDIR/functions.sh" ] && source "$ZDOTDIR/functions.sh"
source_zsh_file "search.sh"
# source_zsh_file "prompt.sh"
# add_zsh_plugin "zsh-users/zsh-autosuggestions"
# add_zsh_plugin "zsh-users/zsh-syntax-highlighting"

source ~/.config/zsh/plugins/powerlevel10k/powerlevel10k.zsh-theme


# uncomment for space before and after output.
# preexec() {
# 
#     echo ""
# 
# }
# 
# precmd() {
#     if [ -z "$OPTIONAL_NEW_LINE" ]; then
#         OPTIONAL_NEW_LINE=1
#     elif [ "$OPTIONAL_NEW_LINE" -eq 1 ]; then
#         echo "\n"
#     fi
# }

# copied from the arch wiki
typeset -g -A key
key[Home]="${terminfo[khome]}"
key[End]="${terminfo[kend]}"
key[Insert]="${terminfo[kich1]}"
key[Backspace]="${terminfo[kbs]}"
key[Delete]="${terminfo[kdch1]}"
key[Up]="${terminfo[kcuu1]}"
key[Down]="${terminfo[kcud1]}"
key[Left]="${terminfo[kcub1]}"
key[Right]="${terminfo[kcuf1]}"
key[PageUp]="${terminfo[kpp]}"
key[PageDown]="${terminfo[knp]}"
key[Shift-Tab]="${terminfo[kcbt]}"

[[ -n "${key[Home]}"      ]] && bindkey -- "${key[Home]}"       beginning-of-line
[[ -n "${key[End]}"       ]] && bindkey -- "${key[End]}"        end-of-line
[[ -n "${key[Insert]}"    ]] && bindkey -- "${key[Insert]}"     overwrite-mode
[[ -n "${key[Backspace]}" ]] && bindkey -- "${key[Backspace]}"  backward-delete-char
[[ -n "${key[Delete]}"    ]] && bindkey -- "${key[Delete]}"     delete-char
[[ -n "${key[Up]}"        ]] && bindkey -- "${key[Up]}"         up-line-or-history
[[ -n "${key[Down]}"      ]] && bindkey -- "${key[Down]}"       down-line-or-history
[[ -n "${key[Left]}"      ]] && bindkey -- "${key[Left]}"       backward-char
[[ -n "${key[Right]}"     ]] && bindkey -- "${key[Right]}"      forward-char
[[ -n "${key[PageUp]}"    ]] && bindkey -- "${key[PageUp]}"     beginning-of-buffer-or-history
[[ -n "${key[PageDown]}"  ]] && bindkey -- "${key[PageDown]}"   end-of-buffer-or-history
[[ -n "${key[Shift-Tab]}" ]] && bindkey -- "${key[Shift-Tab]}"  reverse-menu-complete

if (( ${+terminfo[smkx]} && ${+terminfo[rmkx]} )); then
    autoload -Uz add-zle-hook-widget
    function zle_application_mode_start { echoti smkx }
    function zle_application_mode_stop { echoti rmkx }
    add-zle-hook-widget -Uz zle-line-init zle_application_mode_start
    add-zle-hook-widget -Uz zle-line-finish zle_application_mode_stop
fi
