function go-back-to-dir () {
    cd ..
    zle reset-prompt
}

zle -N go-back-to-dir

bindkey '^U' go-back-to-dir
