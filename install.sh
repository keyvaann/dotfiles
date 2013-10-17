#!/bin/bash
currDir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
ln -vsb $currDir/bashrc ~/.bashrc
ln -vsb $currDir/bash_aliases ~/.bash_aliases
ln -vsb $currDir/conkyrc ~/.conkyrc
ln -vsb $currDir/dircolors ~/.dircolors
ln -vsb $currDir/git-completion.sh ~/.git-completion.sh
ln -vsb $currDir/gitconfig ~/.gitconfig
ln -vsb $currDir/gitignore ~/.gitignore
ln -vsb $currDir/screenrc ~/.screenrc
sudo ln -vsb $currDir/bashrc /root/.bashrc
sudo ln -vsb $currDir/bash_aliases /root/.bash_aliases
sudo ln -vsb $currDir/conkyrc /root/.conkyrc
sudo ln -vsb $currDir/dircolors /root/.dircolors
sudo ln -vsb $currDir/git-completion.sh /root/.git-completion.sh
sudo ln -vsb $currDir/gitconfig /root/.gitconfig
sudo ln -vsb $currDir/screenrc /root/.screenrc
sudo ln -vsb ~/.bash_history /root/.bash_history
mkdir ~/.mplayer/
mkfifo ~/.mplayer/inputpipe
ln -vsb $currDir/mplayer/config ~/.mplayer/config
ln -vsb $currDir/mplayer/input.conf ~/.mplayer/input.conf