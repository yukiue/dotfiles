#!/bin/sh

DOTPATH=~/.dotfiles

for dotfile in .??*
do
    [ "$file" = ".git" ] && continue

    ln -snfv "$DOTPATH/$file" "$HOME"/"$file"
done
