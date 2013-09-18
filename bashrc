#
# ~/.bashrc
# Some of these got from https://github.com/durdn/cfg/blob/master/.bashrc

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Force ignoredups and ignorespace and increace history file size
HISTCONTROL=ignoreboth
export HISTFILESIZE=9999
export HISTSIZE=9999
#make sure the history is updated at every command
export PROMPT_COMMAND="history -a; history -n;"

# For personal use, you can remove it.
CDPATH=/run/media/mrgee

# Usefull staff for git
# https://github.com/joejag/dotfiles/blob/master/bash/completion/git
source ~/.git-completion.sh
export GIT_PS1_SHOWDIRTYSTATE=True
export GIT_PS1_SHOWUNTRACKEDFILES=True
export GIT_PS1_SHOWUPSTREAM=verbose

# Colors are good :)
dircolors -b $HOME/.dircolors > /dev/null

#set the terminal type to 256 colors
export TERM=xterm-256color
# Set a fancy prompt
force_color_prompt=yes
color_prompt=yes
PS1="\`if [ \$? = 0 ]; then echo \[\e[33m\]^_^ ; else echo \[\e[31m\]O_O ; fi\`\[\033[0;33m\][\!] \[\033[1;36m\]\u \[\033[32m\]\`__git_ps1 '(%s)'\` \w\n\[\033[1;36m\]-> \[\033[1;34m\]"
unset color_prompt force_color_prompt

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi
if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
fi

export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

# Some useful functions
# Also see http://gotux.net/arch-linux/custom-bash-commands-functions/

# Extract almost any archive file
extract() {
    local c e i

    (($#)) || return

    for i; do
        c=''
        e=1

        if [[ ! -r $i ]]; then
            echo "$0: file is unreadable: \`$i'" >&2
            continue
        fi

        case $i in
        *.t@(gz|lz|xz|b@(2|z?(2))|a@(z|r?(.@(Z|bz?(2)|gz|lzma|xz)))))
               c='bsdtar xvf';;
        *.7z)  c='7z x';;
        *.Z)   c='uncompress';;
        *.bz2) c='bunzip2';;
        *.exe) c='cabextract';;
        *.gz)  c='gunzip';;
        *.rar) c='unrar x';;
        *.xz)  c='unxz';;
        *.zip) c='unzip';;
        *)     echo "$0: unrecognized file extension: \`$i'" >&2
               continue;;
        esac

        command $c "$i"
        e=$?
    done

    return $e
}

# cd and ls in one
cl() {
    if [ -d "$1" ]; then
        cd "$1"
        ls
        else
        echo "bash: cl: '$1': Directory not found"
    fi
}

dirsize ()
{
    du -shx * .[a-zA-Z0-9_]* 2> /dev/null | \
    egrep '^ *[0-9.]*[MG]' | sort -n > /tmp/list
    egrep '^ *[0-9.]*M' /tmp/list
    egrep '^ *[0-9.]*G' /tmp/list
    rm /tmp/list
}

# do sudo, or sudo the last command if no argument given
s() { 
    if [[ $# == 0 ]]; then
        sudo $(history -p '!!')
    else
        sudo "$@"
    fi
}

#-------------------------------------------------------------
# File & strings related functions:
#-------------------------------------------------------------

# Find a file with a pattern in name:
function ff() { find . -type f -iname '*'"$*"'*' -ls ; }

# Find a file with pattern $1 in name and Execute $2 on it:
function fe() { find . -type f -iname '*'"${1:-}"'*' \
-exec ${2:-file} {} \;  ; }

#  Find a pattern in a set of files and highlight them:
#+ (needs a recent version of egrep).
function fstr()
{
    OPTIND=1
    local mycase=""
    local usage="fstr: find string in files.
Usage: fstr [-i] \"pattern\" [\"filename pattern\"] "
    while getopts :it opt
    do
        case "$opt" in
           i) mycase="-i " ;;
           *) echo "$usage"; return ;;
        esac
    done
    shift $(( $OPTIND - 1 ))
    if [ "$#" -lt 1 ]; then
        echo "$usage"
        return;
    fi
    find . -type f -name "${2:-*}" -print0 | \
xargs -0 egrep --color=always -sn ${case} "$1" 2>&- | more

}

# enquote: surround lines with quotes (useful in pipes) - from mervTormel
enquote () { /usr/bin/sed 's/^/"/;s/$/"/' ; }

function swap()
{ # Swap 2 filenames around, if they exist (from Uzi's bashrc).
    local TMPFILE=tmp.$$

    [ $# -ne 2 ] && echo "swap: 2 arguments needed" && return 1
    [ ! -e $1 ] && echo "swap: $1 does not exist" && return 1
    [ ! -e $2 ] && echo "swap: $2 does not exist" && return 1

    mv "$1" $TMPFILE
    mv "$2" "$1"
    mv $TMPFILE "$2"
}

export EDITOR="vim"
export PAGER="/usr/bin/most -s"
export DISPLAY=:0


################
###Tune Shopt###
################

shopt -s autocd
shopt -u cdable_vars
shopt -s cdspell
shopt -u checkhash
shopt -s checkjobs
shopt -s checkwinsize
shopt -s cmdhist
shopt -u compat31
shopt -u compat32
shopt -u compat40
shopt -u compat41
shopt -s dirspell
shopt -s dotglob
shopt -u execfail
shopt -s expand_aliases
shopt -u extdebug
shopt -s extglob
shopt -s extquote
shopt -u failglob
shopt -s force_fignore
shopt -s globstar
shopt -u gnu_errfmt
shopt -s histappend # Append to the Bash history file, rather than overwriting it
shopt -s histreedit
shopt -s histverify
shopt -u hostcomplete
shopt -u huponexit
shopt -s interactive_comments
shopt -u lastpipe
shopt -u lithist
shopt -u login_shell
shopt -u mailwarn
shopt -u no_empty_cmd_completion
shopt -s nocaseglob
shopt -u nocasematch
shopt -u nullglob
shopt -s progcomp
shopt -s promptvars
shopt -u restricted_shell
shopt -u shift_verbose
shopt -s sourcepath
shopt -u xpg_echo
