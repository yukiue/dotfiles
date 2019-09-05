## ls
alias ls='ls --color=auto'
alias la='ls -a'
alias ll='ls -l'
alias lsf='ls -lF | grep ^-'
alias lsd='ls -lF | grep ^d'

## cd
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias .....='cd ../../../..'
alias ~='cd ~'
alias -- -='cd -'

alias rm='rm -i'
alias mv='mv -i'
alias cp='cp -i'
alias grep='grep --color=auto'

## xsel
alias pbcopy='xsel --clipboard --input'
alias pbpaste='xsel --clipboard --output'

## shortcuts
alias e='emacs'
alias g='git'
alias x='startx'
alias open='xdg-open'

alias remacs='emacs -q'
# alias remacs='emacs -q -l ~/.remacs.d'
alias mew='emacs -e mew'
alias reload='exec $SHELL -l'
alias cdl='cd $(ls -lt | grep ^d | awk '\''NR==1 {print $9}'\'')'
alias xrandr_1920x1080='xrandr --output HDMI-1 --same-as eDP-1 --mode 1920x1080'
alias xrandr_800x600='xrandr --output HDMI-1 --same-as eDP-1 --mode 800x600
'
alias mozc_config='/usr/lib/mozc/mozc_tool --mode=config_dialog'
