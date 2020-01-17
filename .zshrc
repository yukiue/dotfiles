for file in ~/.zsh/*.zsh
do
    source $file
done

#keybind like emacs
bindkey -e

# pip completion
eval "`pip completion --zsh`"
