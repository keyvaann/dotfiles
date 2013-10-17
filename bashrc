# Some of these got from https://github.com/durdn/cfg/blob/master/.bashrc

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Force ignoredups and erase duplicates across the whole history which is good for desktop and bad for server
export HISTCONTROL=ignoredups:erasedups
# Increace history file size
export HISTFILESIZE=9999
export HISTSIZE=2000
# Display TIMESTAMP in history, for auditing purpose
export HISTTIMEFORMAT='%F %T '
# Make sure the history is updated at every command.
export PROMPT_COMMAND='history -a;'

# For personal use, you can remove it.
CDPATH=/run/media/mrgee

export PATH=$PATH:~/.bin

# Usefull staff for git
# https://github.com/joejag/dotfiles/blob/master/bash/completion/git
source ~/.git-completion.sh
export GIT_PS1_SHOWDIRTYSTATE=True
export GIT_PS1_SHOWUNTRACKEDFILES=True
export GIT_PS1_SHOWUPSTREAM=verbose

# Colors are good :)
# https://github.com/trapd00r/LS_COLORS
dircolors -b $HOME/.dircolors > /dev/null

# Set the terminal type to 256 colors
export TERM=xterm-256color

# Set a fancy prompt
export BBrown='\[\033[1\;33m\]';
export BRed='\[\033[1\;31m\]';
export Brown='\[\033[0;33m\]';
export BCyan='\[\033[1;36m\]';
export BGreen='\[\033[1;32m\]';
export BBlue='\[\033[1;34m\]';
export White='\[\033[0;38m\]';
PS1="\$(if [ \$? = 0 ]; then echo $BBrown\:\) ; else echo $BRed\:\( ; fi) $Brown[\!] $BCyan\u $BGreen\w $White\@ \$(__git_ps1 '(%s)')\n$BCyan-> $BBlue"

[ -r ~/.bash_aliases ] && . ~/.bash_aliases

# Try to enable the auto-completion (type: "pacman -S bash-completion" to install it).
[ -r /usr/share/bash-completion/bash_completion ] && . /usr/share/bash-completion/bash_completion

# Try to enable the "Command not found" hook ("pacman -S pkgfile" to install it).
# See also: https://wiki.archlinux.org/index.php/Bash#The_.22command_not_found.22_hook
[ -r /usr/share/doc/pkgfile/command-not-found.bash ] && . /usr/share/doc/pkgfile/command-not-found.bash

# Add tab completion for SSH hostnames based on ~/.ssh/config, ignoring wildcards
[ -e "$HOME/.ssh/config" ] && complete -o "default" -o "nospace" -W "$(grep "^Host" ~/.ssh/config | grep -v "[?*]" | cut -d " " -f2 | tr ' ' '\n')" scp sftp ssh

export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export EDITOR='emacsclient -a ""'
export PAGER="/usr/bin/most -s"
export DISPLAY=:0

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

# Get a character’s Unicode code point
function codepoint() {
        perl -e "use utf8; print sprintf('U+%04X', ord(\"$@\"))"
        # print a newline unless we’re piping the output to another program
        if [ -t 1 ]; then
                echo # newline
        fi
}

# Show all the names (CNs and SANs) listed in the SSL certificate
# for a given domain
function getcertnames() {
        if [ -z "${1}" ]; then
                echo "ERROR: No domain specified."
                return 1
        fi

        local domain="${1}"
        echo "Testing ${domain}…"
        echo # newline

        local tmp=$(echo -e "GET / HTTP/1.0\nEOT" \
                | openssl s_client -connect "${domain}:443" 2>&1);

        if [[ "${tmp}" = *"-----BEGIN CERTIFICATE-----"* ]]; then
                local certText=$(echo "${tmp}" \
                        | openssl x509 -text -certopt "no_header, no_serial, no_version, \
                        no_signame, no_validity, no_issuer, no_pubkey, no_sigdump, no_aux");
                        echo "Common Name:"
                        echo # newline
                        echo "${certText}" | grep "Subject:" | sed -e "s/^.*CN=//";
                        echo # newline
                        echo "Subject Alternative Name(s):"
                        echo # newline
                        echo "${certText}" | grep -A 1 "Subject Alternative Name:" \
                                | sed -e "2s/DNS://g" -e "s/ //g" | tr "," "\n" | tail -n +2
                        return 0
        else
                echo "ERROR: Certificate not found.";
                return 1
        fi
}

# Cd and ls in one
cl() {
    if [ -d "$1" ]; then
        cd "$1"
        ls
        else
        echo "bash: cl: '$1': Directory not found"
    fi
}

# Determine size of a file or total size of a directory
function fs() {
        if du -b /dev/null > /dev/null 2>&1; then
                local arg=-sbh
        else
                local arg=-sh
        fi
        if [[ -n "$@" ]]; then
                du $arg -- "$@"
        else
                du $arg .[^.]* *
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

# Do sudo, or sudo the last command if no argument given
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

# Enquote: surround lines with quotes (useful in pipes) - from mervTormel
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

################
###Tune Shopt###
################

shopt -s autocd
shopt -u cdable_vars
shopt -s cdspell # Autocorrect typos in path names when using `cd`
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
shopt -s nocaseglob # Case-insensitive globbing (used in pathname expansion)
shopt -u nocasematch
shopt -u nullglob
shopt -s progcomp
shopt -s promptvars
shopt -u restricted_shell
shopt -u shift_verbose
shopt -s sourcepath
shopt -u xpg_echo