export PATH="$PATH:$HOME/bin"

# go
export GOPATH="$HOME/go"
export PATH="$PATH:$HOME/go/bin"

# python
export PYTHONPATH="$HOME/lib/python"

# anyenv
if [ -d $HOME/.anyenv ]; then
    export PATH="$HOME/.anyenv/bin:$PATH"
    eval "$(anyenv init -)"
fi

# latex
export TEXINPUTS=.:~/lib/texmf//:/usr/share/texlive/texmf-dist//:/etc/texmf//:/usr/share/texmf/tex/latex/lm//

# editor
export EDITOR=emacs

export LSCOLORS=exfxcxdxbxegedabagacad
export LS_COLORS='di=34:ln=35:so=32:pi=33:ex=31:bd=46;34:cd=43;34:su=41;30:sg=46;30:tw=42;30:ow=43;30'

export HISTFILE=~/.zsh_history
export HISTSIZE=10000
export SAVEHIST=1000000
