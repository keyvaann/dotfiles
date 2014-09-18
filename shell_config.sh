# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# exports =======================================
export CDPATH=/run/media/mrgee:~/git:/run/media/mrgee/d/torrent/downloads
export PYTHONSTARTUP=$HOME/.pythonrc.py
export PIP_DOWNLOAD_CACHE=$HOME/.cache/pip
export PYENV_ROOT="$HOME/.pyenv"
export PATH=~/.bin:~/.rbenv/bin:$PYENV_ROOT/bin:/usr/local/heroku/bin:$PATH
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export EDITOR='emacsclient -a ""'
export PAGER="/usr/bin/most -s"
export DISPLAY=:0

# Git stuff
export GIT_PS1_SHOWDIRTYSTATE=True
export GIT_PS1_SHOWSTASHSTATE=True
export GIT_PS1_SHOWUNTRACKEDFILES=True
export GIT_PS1_SHOWCOLORHINTS=True
export GIT_PS1_SHOWUPSTREAM=verbose
export GIT_PS1_DESCRIBE_STYLE=branch

# Security: close root shells after n seconds of inactivity
[ "$UID" = 0 ] && export TMOUT=180

# automatically log out after 2 hours of inactivity
#export TMOUT=7200

# virtualenvwrapper
# export WORKON_HOME=$HOME/.virtualenvs
# export PROJECT_HOME=$HOME/git
# source /usr/bin/virtualenvwrapper.sh
# source ~/.autoenv/activate.sh

# eval & source =================================
# Colors are good :)
# https://github.com/trapd00r/LS_COLORS
dircolors -b ~/.dircolors > /dev/null

[ -r ~/.dotfiles/shell_aliases.sh ] && source ~/.dotfiles/shell_aliases.sh
[ -r ~/.shell_functions.sh ] && source ~/.shell_functions.sh

[[ -s "$HOME/.rbenv/bin/rbenv" ]] && eval "$(rbenv init -)"
[[ -s "$HOME/.pyenv/bin/pyenv" ]] && eval "$(pyenv init -)"

eval $(keychain --eval --agents ssh -Q --quiet id_rsa)

# Usefull staff for git
# https://github.com/git/git/blob/master/contrib/completion
source ~/.git-prompt.sh
if [ $(/bin/ps | grep $$ | awk '{print $4}') = 'bash' ]; then
    source ~/.git-completion.bash
fi

# Safe default permissions
#umask 077

# Add tab completion for SSH hostnames based on ~/.ssh/config, ignoring wildcards
[ -e "$HOME/.ssh/config" ] && complete -o "default" -o "nospace" -W "$(grep "^Host" ~/.ssh/config | grep -v "[?*]" | cut -d " " -f2 | tr ' ' '\n')" scp sftp ssh


# ulimit settings are per-process, 'man bash', not 'man ulimit'
ulimit -c 0     # create no core files
ulimit -m 500000
