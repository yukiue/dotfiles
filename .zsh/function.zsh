function history-all { history -E 1 }

# function ghq-fzf() {
#     local selected_dir=$(ghq list | fzf --query="$LBUFFER")

#     if [ -n "$selected_dir" ]; then
#         BUFFER="cd $(ghq root)/${selected_dir}"
#         zle accept-line
#     fi

#     zle reset-prompt
# }

# zle -N ghq-fzf

# bindkey "^g" ghq-fzf

function chpwd() { ls }
