for file in ~/.zsh/*.zsh
do
    source $file
done


PROMPT='%F{green}%n%f@%F{cyan}%m%f:%F{red}%d%f
%# '

setopt auto_pushd
setopt pushd_ignore_dups
# setopt GLOBDOTS
setopt correct
# setopt correct_all
setopt no_beep
# setopt always_last_prompt


zstyle ':completion:*:default' menu select=2

# エイリアスでコマンドラインの自動補完を切り替える
setopt completealiases

# ls after change directory
function chpwd() { ls }

# 他のターミナルとヒストリーを共有
setopt share_history
# setopt Hist_Ignore_Dups
setopt hist_ignore_dups
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
# path=($HOME/bin(N-/) $path)

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



# ブランチ名を色付きで表示させるメソッド
function rprompt-git-current-branch {
  local branch_name st branch_status

  if [ ! -e  ".git" ]; then
    # gitで管理されていないディレクトリは何も返さない
    return
  fi
  branch_name=`git rev-parse --abbrev-ref HEAD 2> /dev/null`
  st=`git status 2> /dev/null`
  if [[ -n `echo "$st" | grep "^nothing to"` ]]; then
    # 全てcommitされてクリーンな状態
    branch_status="%F{cyan}"
  elif [[ -n `echo "$st" | grep "^Untracked files"` ]]; then
    # gitに管理されていないファイルがある状態
    branch_status="%F{red}?"
  elif [[ -n `echo "$st" | grep "^Changes not staged for commit"` ]]; then
    # git addされていないファイルがある状態
    branch_status="%F{red}+"
  elif [[ -n `echo "$st" | grep "^Changes to be committed"` ]]; then
    # git commitされていないファイルがある状態
    branch_status="%F{yellow}!"
  elif [[ -n `echo "$st" | grep "^rebase in progress"` ]]; then
    # コンフリクトが起こった状態
    echo "%F{red}!(no branch)"
    return
  else
    # 上記以外の状態の場合は青色で表示させる
    branch_status="%F{blue}"
  fi
  # ブランチ名を色付きで表示する
  echo "${branch_status}[$branch_name]"
}

# # プロンプトが表示されるたびにプロンプト文字列を評価、置換する
setopt prompt_subst

# # プロンプトの右側(RPROMPT)にメソッドの結果を表示させる
RPROMPT='`rprompt-git-current-branch`'

# autoload -Uz vcs_info
# zstyle ':vcs_info:*' formats '[%b]'
# zstyle ':vcs_info:*' actionformats '[%b|%a]'
# precmd () {
#     psvar=()
#     LANG=en_US.UTF-8 vcs_info
#     [[ -n "$vcs_info_msg_0_" ]] && psvar[1]="$vcs_info_msg_0_"
# }
# RPROMPT="%1(v|%F{yellow}%1v%f|)"


# pip completion
eval "`pip completion --zsh`"

# # pip zsh completion start
# function _pip_completion {
#   local words cword
#   read -Ac words
#   read -cn cword
#   reply=( $( COMP_WORDS="$words[*]" \
#              COMP_CWORD=$(( cword-1 )) \
#              PIP_AUTO_COMPLETE=1 $words[1] 2>/dev/null ))
# }
# compctl -K _pip_completion pip
# # pip zsh completion end

