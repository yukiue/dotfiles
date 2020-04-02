# ls
alias l='ls --color=auto'
alias ls='ls --color=auto'
alias la='ls -A'
alias ll='ls -lh'
alias lsf='ls -lF | grep ^-'
alias lsd='ls -lF | grep ^d'


# cd
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias .....='cd ../../../..'
alias ~='cd ~'
alias cdl='cd $(ls -lt | grep ^d | awk '\''NR==1 {print $9}'\'')'


# shortcut
alias g='git'
alias x='startx'
alias open='xdg-open'


# basic aliases
alias rm='rm -ir'
alias mv='mv -i'
alias cp='cp -i'
alias grep='grep --color=auto'
alias mkdir='mkdir -p'
alias reload='exec $SHELL -l'


# xsel
alias pbcopy='xsel --clipboard --input'
alias pbpaste='xsel --clipboard --output'


# emacs
alias ec='emacsclient'
alias ee='emacs -nw'
# alias oemacs='emacs -q'
alias oemacs='emacs -q -l ~/.emacs.conf'
alias en='emacsclient -n'
alias e='emacsclient -e "(elscreen-create)" > /dev/null && emacsclient -n'
# alias e='(){pidof emacs > /dev/null && ecn $1 || emacs $1 &}'
alias mew='emacsclient -e "(elscreen-create)" && emacsclient -e "(mew)"'
alias cde='cd $(emacsclient -e "(return-current-working-directory-to-shell)" | sed s/\"//g)'


# xrandr
alias xrandr_1920x1080_HDMI='xrandr --output HDMI-1 --same-as eDP-1 --mode 1920x1080'
alias xrandr_1920x1080_DP='xrandr --output DP-1 --same-as eDP-1 --mode 1920x1080'


# misc
alias mozc='/usr/lib/mozc/mozc_tool --mode=config_dialog'
# alias mupdf='(){mupdf $1 2> /dev/null}'
# alias chromium='(){chromium $1 2> /dev/null }'
alias utf8='nkf -w --overwrite'
alias range='(){head -$2 | tail -`expr $2 - $1 + 1`}'
alias localhost='python -m http.server'
