alias l='ls --color=auto'
alias ls='ls --color=auto'
alias la='ls -a'
alias ll='ls -l'
alias lsf='ls -lF | grep ^-'
alias lsd='ls -lF | grep ^d'

alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias .....='cd ../../../..'
alias ~='cd ~'

alias rm='rm -i'
alias mv='mv -i'
alias cp='cp -i'
alias grep='grep --color=auto'
alias mkdir='mkdir -p'
alias pbcopy='xsel --clipboard --input'
alias pbpaste='xsel --clipboard --output'

alias e='emacs'
alias g='git'
alias x='startx'
alias open='xdg-open'

# emacs
alias remacs='emacs -q'
# alias remacs='emacs -q -l ~/.remacs.d'
alias ec='emacsclient -e "(elscreen-create)" > /dev/null && emacsclient -n'
alias ecn='emacsclient -n'
alias e='emacs -nw'
# alias mew='emacs -e mew'
alias mew='emacsclient -e "(elscreen-create)" && emacsclient -e "(mew)"'
# alias cde='cd $(emacsclient -e "(return-current-working-directory-to-shell)" | sed 's/^"\(.*\)"$/\1/')'

alias reload='exec $SHELL -l'
alias cdl='cd $(ls -lt | grep ^d | awk '\''NR==1 {print $9}'\'')'
alias xrandr_1920x1080_HDMI='xrandr --output HDMI-1 --same-as eDP-1 --mode 1920x1080'
alias xrandr_800x600_HDMI='xrandr --output HDMI-1 --same-as eDP-1 --mode 800x600'
alias xrandr_1920x1080_DP='xrandr --output DP-1 --same-as eDP-1 --mode 1920x1080'
alias xrandr_800x600_DP='xrandr --output DP-1 --same-as eDP-1 --mode 800x600'
alias mozc='/usr/lib/mozc/mozc_tool --mode=config_dialog'

function cde () {
    EMACS_CWD=`emacsclient -e "(return-current-working-directory-to-shell)" | sed 's/^"\(.*\)"$/\1/'`
    echo "cd $EMACS_CWD"
    cd "$EMACS_CWD"
}
