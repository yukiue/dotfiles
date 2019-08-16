for file in ~/.zsh/*.zsh
do
    source $file
done


PROMPT='%F{green}%n@%m%f:%F{red}%d%f
%# '


# 他のターミナルとヒストリーを共有
setopt share_history
HISTFILE=~/.zsh_history
HISTSIZE=100000
#HISTTIMEFORMAT='%Y/%m/%d %H:%M:%'

SAVEHIST=100000

function history-all { history -E 1 } 

# ls
export LSCOLORS=exfxcxdxbxegedabagacad
export LS_COLORS='di=34:ln=35:so=32:pi=33:ex=31:bd=46;34:cd=43;34:su=41;30:sg=46;30:tw=42;30:ow=43;30'

# 補完候補もLS_COLORSに合わせて色が付くようにする
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}


#path
path=($HOME/bin(N-/) $path)

#keybind like emacs1
bindkey -e
 
#if test -z "$DISPLAY"; then
#    setfont /usr/share/consolefonts/CrySlav-Fixed18.psf.gz
#fi

export GOPATH="$HOME/go"
export PATH="$PATH:$HOME/go/bin"
export PYTHONPATH="$HOME/lib/python"

function ghq-fzf() {
    local selected_dir=$(ghq list | fzf --query="$LBUFFER")

    if [ -n "$selected_dir" ]; then
        BUFFER="cd $(ghq root)/${selected_dir}"
        zle accept-line
    fi

    zle reset-prompt
}

zle -N ghq-fzf

bindkey "^g" ghq-fzf

export TEXINPUTS=.:~/lib/texmf//:/usr/share/texlive/texmf-dist//:/etc/texmf//
