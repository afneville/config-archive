function source_zsh_file() {

    [ -f "$ZDOTDIR/$1" ] && source "$ZDOTDIR/$1"

}

function add_zsh_plugin() {

    PLUGIN_NAME=$(echo $1 | cut -d "/" -f 2)

    if [ -d "$ZDOTDIR/plugins/$PLUGIN_NAME" ]; then 

        source_zsh_file "plugins/$PLUGIN_NAME/$PLUGIN_NAME.plugin.zsh" || \
        source_zsh_file "plugins/$PLUGIN_NAME/$PLUGIN_NAME.zsh-theme" || \
        source_zsh_file "plugins/$PLUGIN_NAME/$PLUGIN_NAME.zsh"

    else

        git clone "https://github.com/$1.git" "$ZDOTDIR/plugins/$PLUGIN_NAME"

    fi

}
