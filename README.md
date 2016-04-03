###Description
My linux config files, put them here for backup and sharing.

###Installation

#####General
```shell
mkdir -p ~/.bin ~/.config/htop/
ln -s $HOME/.dotfiles/curlrc $HOME/.curlrc
ln -s $HOME/.dotfiles/flake8rc $HOME/.config/flake8
ln -s $HOME/.dotfiles/gitconfig $HOME/.gitconfig
ln -s $HOME/.dotfiles/gitignore $HOME/.gitignore
ln -s $HOME/.dotfiles/htoprc $HOME/.config/htop/htoprc
ln -s $HOME/.dotfiles/nanorc $HOME/.nanorc
ln -s $HOME/.dotfiles/psqlrc $HOME/.psqlrc
ln -s $HOME/.dotfiles/ssh_config $HOME/.ssh/config
ln -s $HOME/.dotfiles/wgetrc $HOME/.wgetrc
```

#####i3
Put a background image in `~/.i3/background.png`
```shell
apt-get install kbdd
ln -s $HOME/.dotfiles/i3_config /$HOME/.i3/config
ln -s $HOME/.dotfiles/Xmodmap $HOME/.Xmodmap
```

#####Powerline font
```shell
git clone https://github.com/gabrielelana/awesome-terminal-fonts
cp awesome-terminal-fonts/build/* ~/.local/share/fonts/
fc-cache -fv ~/.local/share/fonts/
ln -s $HOME/.dotfiles/fontconfig-symbols.conf $HOME/.config/fontconfig/conf.d/10-symbols.conf
```

#####ZSH
```shell

git clone git://github.com/robbyrussell/oh-my-zsh.git ~/.oh-my-zsh
cp ~/.zshrc ~/.zshrc.orig
ln -s $HOME/.dotfiles/zshrc $HOME/.zshrc
sudo chsh -s /bin/zsh
git clone https://github.com/bhilburn/powerlevel9k.git $ZSH_CUSTOM/themes/powerlevel9k
git clone git://github.com/zsh-users/zsh-autosuggestions $ZSH_CUSTOM/plugins/zsh-autosuggestions
git clone https://github.com/zsh-users/zsh-syntax-highlighting.git $ZSH_CUSTOM/plugins/zsh-syntax-highlighting
git clone https://github.com/zsh-users/zsh-completions $ZSH_CUSTOM/plugins/zsh-completions
git clone git://github.com/zsh-users/zaw.git ~/.zaw
```

#####mpv
```shell
mkdir ~/.mpv
ln -s $HOME/.dotfiles/mpv/config $HOME/.mpv/config
ln -s $HOME/.dotfiles/mpv/input.conf $HOME/.mpv/input.conf
```


#####SSHRC
```
git clone https://github.com/Russell91/sshrc.git
cp sshrc/sshrc ~/.bin
ln -s $HOME/.dotfiles/sshrc $HOME/.sshrc
mkdir ~/.sshrc.d/
ln -s $HOME/.dotfiles/nanorc $HOME/.sshrc.d
```

### Other sources

https://github.com/webpro/awesome-dotfiles

http://dotfiles.org/

https://github.com/mathiasbynens/dotfiles

https://github.com/mitsuhiko/dotfiles

https://github.com/trapd00r/configs

https://gist.github.com/oli/1637874

https://gist.github.com/pksunkara/988716

https://git.wiki.kernel.org/index.php/Aliases

https://github.com/examon/dotfiles

https://github.com/kooothor/.dotfiles

https://github.com/mitsuhiko/dotfiles

*http://git.onerussian.com/?p=etc/bash.git;a=summary

http://git.onerussian.com/?p=etc/base.git;a=summary
