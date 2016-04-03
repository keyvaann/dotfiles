###Description
My linux config files, put them here for backup and sharing.

###Installation

#####ZSH
```shell

git clone git://github.com/robbyrussell/oh-my-zsh.git ~/.oh-my-zsh
cp ~/.zshrc ~/.zshrc.orig
ln -s /home/$USER/.dotfiles/zshrc /home/$USER/.zshrc
sudo chsh -s /bin/zsh
git clone git://github.com/zsh-users/zsh-autosuggestions $ZSH_CUSTOM/plugins/zsh-autosuggestions
git clone https://github.com/zsh-users/zsh-syntax-highlighting.git $ZSH_CUSTOM/plugins/zsh-syntax-highlighting
git clone https://github.com/zsh-users/zsh-completions $ZSH_CUSTOM/plugins/zsh-completions
git clone git://github.com/zsh-users/zaw.git ~/.zaw
```

### Other sources
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
