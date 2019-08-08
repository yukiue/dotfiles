#!/bin/sh

DOTPATH=~/.dotfiles

for dotfile in .??*
do
    [ "$dotfile" = ".git" ] && continue

    ln -snfv "$DOTPATH/$dotfile" "$HOME"/"$dotfile"
done
