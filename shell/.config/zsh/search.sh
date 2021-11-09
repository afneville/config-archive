#!/bin/sh

function fd() {

    choice=$(echo "$(find ~/* \( -name '.git' -o -name 'env' -o -wholename '/home/alex/code' \) -prune -false -o -type d)\nquit" | fzf)
    if [[ $choice != "quit" ]]; then

        pushd $choice

    fi
}

function ff(){

    choice=$(echo "$(find ~/* \( -name '.git' -o -name 'env' -o -wholename '/home/alex/code' \) -prune -false -o -type f)\nquit" | fzf)

    if [[ ! -z "$choice" ]]; then

        if [[ $choice != "quit" ]]; then

            pushd $(dirname $choice)
            nvim $choice
            popd

        fi

    fi

}

function f.(){

    choice=$(echo "$(find ./* \( -name '.git' -o -name 'env' -o -wholename '/home/alex/code' \) -prune -false -o -type f)\nquit" | fzf)

    if [[ ! -z "$choice" ]]; then

        if [[ $choice != "quit" ]]; then

            nvim $choice

        fi

    fi

}
