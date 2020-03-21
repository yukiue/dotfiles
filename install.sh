#!/bin/sh

DOTPATH=~/.dotfiles

for dotfile in `find . -mindepth 1 -maxdepth 1 -name ".*"  -exec basename {} \;`
do
    [ "$dotfile" = ".git" ] && continue
    [ "$dotfile" = ".config" ] && continue
    ln -snfv "$DOTPATH/$dotfile" "$HOME/$dotfile"
done

mkdir -p ~/.config

for dotfile in `find ./.config -mindepth 1 -maxdepth 1 -exec basename {} \;`
do
    ln -snfv "$DOTPATH/.config/$dotfile" "$HOME/.config/$dotfile"
done
