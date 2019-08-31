#!/bin/sh

DOTPATH=~/.dotfiles

for dotfile in .??*
do
    [ "$dotfile" = ".git" ] && continue
    [ "$dotfile" = ".gitconfig" ] && continue

    ln -snfv "$DOTPATH/$dotfile" "$HOME"/"$dotfile"
done
