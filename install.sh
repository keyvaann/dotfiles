#!/bin/bash
CurrDir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
ln -vsb $CurrDir/bashrc ~/.bashrc
ln -vsb $CurrDir/bash_aliases ~/.bash_aliases
ln -vsb $CurrDir/conkyrc ~/.conkyrc
ln -vsb $CurrDir/dircolors ~/.dircolors
ln -vsb $CurrDir/git-completion.sh ~/.git-completion.sh
ln -vsb $CurrDir/gitconfig ~/.gitconfig
ln -vsb $CurrDir/screenrc ~/.screenrc
mkdir ~/.mplayer/
mkfifo ~/.mplayer/inputpipe
ln -vsb $CurrDir/mplayer/config ~/.mplayer/config
ln -vsb $CurrDir/mplayer/input.conf ~/.mplayer/input.conf
