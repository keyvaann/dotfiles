# Description

My linux config files, put them here for backup and sharing.

# Installation

## General

```shell
sudo apt install git curl
git clone https://github.com/K1Hyve/dotfiles.git ~/.dotfiles
sudo sh -c "$(curl --location https://taskfile.dev/install.sh)" -- -d -b /usr/local/bin
cd ~/.dotfiles
task install-all
```

## Emacs

Uncomment source lines in sources.list and update apt

```shell
sudo apt-get install silversearcher-ag
sudo apt-get build-dep emacs24
wget http://mirrors.peers.community/mirrors/gnu/emacs/emacs-25.2.tar.xz
tar -xJf emacs-25.2.tar.xz
./configure --with-xft --with-x-toolkit=lucid --with-modules && make && sudo make install

curl -L https://git.io/epre | sh
ln -s $HOME/.dotfiles/emacs/config.el ~/.emacs.d/personal/config.el
ln -s $HOME/.dotfiles/emacs/custom.el  ~/.emacs.d/personal/custom.el
ln -s $HOME/.dotfiles/emacs/prelude-modules.el $HOME/.emacs.d/prelude-modules.el
```

Due to some bug we need to install Jedi version 0.9 as instructed below

```shell
git clone https://github.com/davidhalter/jedi
cd jedi
git checkout v0.9.0
python setup.py sdist
cd dist
PYTHONPATH=$HOME/.emacs.d/anaconda-mode/0.1.8 easy_install -d $HOME/.emacs.d/anaconda-mode/0.1.8 -S $HOME/.emacs.d/anaconda-mode/0.1.8 -a -Z jedi-0.9.0.tar.gz
```

## Powerline font

```shell
sudo apt-get install fonts-inconsolata
mkdir -p ~/.local/share/fonts/ $HOME/.config/fontconfig/conf.d/
git clone https://github.com/gabrielelana/awesome-terminal-fonts
cp awesome-terminal-fonts/build/* ~/.local/share/fonts/
fc-cache -fv ~/.local/share/fonts/
ln -s $HOME/.dotfiles/fontconfig-symbols.conf $HOME/.config/fontconfig/conf.d/10-symbols.conf
```

# Other sources

https://github.com/webpro/awesome-dotfiles

https://dotfiles.github.io/

http://dotfiles.org/

http://dotshare.it/

https://github.com/mathiasbynens/dotfiles

https://github.com/mitsuhiko/dotfiles

https://github.com/trapd00r/configs

https://gist.github.com/oli/1637874

https://gist.github.com/pksunkara/988716

https://git.wiki.kernel.org/index.php/Aliases

https://github.com/examon/dotfiles

https://github.com/kooothor/.dotfiles

https://github.com/mitsuhiko/dotfiles

http://git.onerussian.com/?p=etc/bash.git;a=summary

http://git.onerussian.com/?p=etc/base.git;a=summary
