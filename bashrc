# -*- mode: sh; -*-
# Some of these got from https://github.com/durdn/cfg/blob/master/.bashrc

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

source ~/.shell_config.sh

# Force ignoredups and erase duplicates across the whole history which is good for desktop and bad for server
export HISTCONTROL=ignoredups:erasedups
# Increace history file size
export HISTFILESIZE=9999
export HISTSIZE=5000
# Display TIMESTAMP in history, for auditing purpose
export HISTTIMEFORMAT='%F %T '
# Make sure the history is updated at every command.
export PROMPT_COMMAND='history -a; history -n; '

# Safe default permissions
#umask 077

# For personal use, you can remove it.
CDPATH=/run/media/mrgee:~/git

export PYTHONSTARTUP=$HOME/.pythonrc.py
export PYENV_ROOT="$HOME/.pyenv"
export PATH=~/.bin:~/.rbenv/bin:$PYENV_ROOT/bin:/usr/local/heroku/bin:$PATH
eval "$(rbenv init -)"
eval "$(pyenv init -)"
eval $(keychain --eval --agents ssh -Q --quiet id_rsa)

# virtualenvwrapper
# export WORKON_HOME=$HOME/.virtualenvs
# export PROJECT_HOME=$HOME/git
# source /usr/bin/virtualenvwrapper.sh
# source ~/.autoenv/activate.sh

stty -ixon      # disable XON/XOFF flow control (^s/^q)

# replace Cpas Lock with something usefull
# http://www.epiguru.com/2012/06/how-to-remap-the-caps-lock-key-to-the-super-key-on-linux/
xmodmap -e "remove Lock = Caps_Lock" 2> /dev/null
xmodmap -e "keysym Caps_Lock = Super_R" 2> /dev/null

# remove damn useless numlock
# https://unix.stackexchange.com/questions/98068/keep-numlock-always-on
xmodmap -e "keycode 77 ="

# Usefull staff for git
# https://github.com/joejag/dotfiles/blob/master/bash/completion/git
# https://github.com/git/git/blob/master/contrib/completion
source ~/.git-prompt.sh
source ~/.git-completion.bash
export GIT_PS1_SHOWDIRTYSTATE=True
export GIT_PS1_SHOWSTASHSTATE=True
export GIT_PS1_SHOWUNTRACKEDFILES=True
export GIT_PS1_SHOWCOLORHINTS=True
export GIT_PS1_SHOWUPSTREAM=verbose
export GIT_PS1_DESCRIBE_STYLE=branch

# Colors are good :)
# https://github.com/trapd00r/LS_COLORS
dircolors -b ~/.dircolors > /dev/null

# ctrl+w and slashes
stty werase undef
bind '"\C-w": unix-filename-rubout'

stty -ixon      # disable XON/XOFF flow control (^s/^q)

set -o notify          # Don't wait for job termination notification

# Set a fancy prompt
export BBrown='\[\033[1\;33m\]'
export BRed='\[\033[1\;31m\]'
export Brown='\[\033[0;33m\]'
export BCyan='\[\033[1;36m\]'
export BGreen='\[\033[1;32m\]'
export BBlue='\[\033[1;34m\]'
export White='\[\033[0;38m\]'

case ${TERM} in
  xterm*|rxvt*|Eterm|aterm|kterm|gnome*)
    PS1="$Brown[\!] $BCyan\u $BGreen\w $White\@ \$(__git_ps1 '(%s)')\n$BCyan-> $BBlue"
    ;;
  screen)
    PS1="[\!] \u \w \@\n-> "
    ;;
esac

# Try to enable the auto-completion (type: "pacman -S bash-completion" to install it).
[ -r /usr/share/bash-completion/bash_completion ] && . /usr/share/bash-completion/bash_completion

# Try to enable the "Command not found" hook ("pacman -S pkgfile" to install it).
# See also: https://wiki.archlinux.org/index.php/Bash#The_.22command_not_found.22_hook
[ -r /usr/share/doc/pkgfile/command-not-found.bash ] && . /usr/share/doc/pkgfile/command-not-found.bash

# ^d must be pressed twice to exit shell
export IGNOREEOF=1

# Tab (readline) completion settings
set completion-ignore-case on # Set tab-completion to be case-insensitive
set match-hidden-files on
set show-all-if-ambiguous on # Show all tab-completed matches
set show-all-symlinked-directories on # Set all symlinked-directories to be shown

#http://bash.blogsky.com/1392/09/13/post-213/%D9%BE%DB%8C%D8%B4%E2%80%8C%DA%AF%DB%8C%D8%B1%DB%8C-%D8%A7%D8%B2-%D8%B1%D9%88%D9%86%D9%88%DB%8C%D8%B3%DB%8C-%D9%86%D8%A7%D8%AE%D9%88%D8%A7%D8%B3%D8%AA%D9%87-%D9%81%D8%A7%DB%8C%D9%84
set -o noclobber

#stops ctrl+d from logging me out
#set -o ignoreeof

export EDITOR='emacsclient -a ""'
alias e='~/git/snippets/runemacs.sh'
alias ec='emacsclient -t -a ""'

eval $(keychain --eval --agents ssh -Q --quiet id_rsa)

# Add tab completion for SSH hostnames based on ~/.ssh/config, ignoring wildcards
[ -e "$HOME/.ssh/config" ] && complete -o "default" -o "nospace" -W "$(grep "^Host" ~/.ssh/config | grep -v "[?*]" | cut -d " " -f2 | tr ' ' '\n')" scp sftp ssh


# Usefull staff for git
# https://github.com/git/git/blob/master/contrib/completion
source ~/.git-prompt.sh
if [ $(/bin/ps | grep $$ | awk '{print $4}') = 'bash' ]; then
    source ~/.git-completion.bash
        fi


################
###Tune Shopt###
################

shopt -s autocd
shopt -s cdable_vars # if cd arg is not valid, assumes its a var defining a dir
shopt -s cdspell # Autocorrect typos in path names when using `cd`
shopt -u checkhash
shopt -s checkjobs
shopt -s checkwinsize # check the window size after each command and, if necessary, update the values of LINES and COLUMNS.
shopt -s cmdhist # multiline commands saved in history as oneliners
shopt -u compat31
shopt -u compat32
shopt -u compat40
shopt -u compat41
shopt -s dirspell
shopt -s dotglob # files beginning with . to be returned in the results of path-name expansion.
shopt -s execfail # failed execs don't exit shell
shopt -s expand_aliases # expand aliases
shopt -u extdebug
shopt -s extglob # bonus regex globbing!
shopt -s extquote
shopt -u failglob
shopt -u force_fignore # Expand to complete an ignored word, if no other words match.
shopt -s globstar
shopt -u gnu_errfmt
shopt -s histappend # Append to the Bash history file, rather than overwriting it
shopt -s histreedit # allow re-editing of a history command substitution, if the previous run failed
shopt -s histverify # good for double-checking history substitutions
shopt -s hostcomplete # tab-complete words containing @ as hostnames
shopt -u huponexit
shopt -s interactive_comments
shopt -u lastpipe
shopt -u lithist
shopt -u login_shell
shopt -u mailwarn # No messages about new e-mail
shopt -s no_empty_cmd_completion # Don't try to find all the command possibilities when hitting TAB on an empty line.
shopt -s nocaseglob # Case-insensitive globbing (used in pathname expansion)
shopt -u nocasematch
shopt -u nullglob
shopt -s progcomp
shopt -s promptvars
shopt -u restricted_shell
shopt -u shift_verbose
shopt -s sourcepath
shopt -u xpg_echo


# ----------------------------------------------------------------------
# ifconfig completes to interfaces output by 'ifconfig -a'.

_ifconfig()
{
    COMPREPLY=( $( compgen -W '$( ifconfig -a | perl -lne"print \$1 if /^(\w+):/" )' -- ${COMP_WORDS[COMP_CWORD]} ) )
    return 0
}
complete -F _ifconfig ifconfig
