# Some of these got from https://github.com/durdn/cfg/blob/master/.bashrc

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

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

# Set the terminal type to 256 colors
#export TERM=xterm-256color

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

[ -r ~/.bash_aliases ] && . ~/.bash_aliases

# Try to enable the auto-completion (type: "pacman -S bash-completion" to install it).
[ -r /usr/share/bash-completion/bash_completion ] && . /usr/share/bash-completion/bash_completion

# Try to enable the "Command not found" hook ("pacman -S pkgfile" to install it).
# See also: https://wiki.archlinux.org/index.php/Bash#The_.22command_not_found.22_hook
[ -r /usr/share/doc/pkgfile/command-not-found.bash ] && . /usr/share/doc/pkgfile/command-not-found.bash

# Add tab completion for SSH hostnames based on ~/.ssh/config, ignoring wildcards
[ -e "$HOME/.ssh/config" ] && complete -o "default" -o "nospace" -W "$(grep "^Host" ~/.ssh/config | grep -v "[?*]" | cut -d " " -f2 | tr ' ' '\n')" scp sftp ssh

# automatically log out after 2 hours of inactivity
#export TMOUT=7200

# Security: close root shells after n seconds of inactivity
[ "$UID" = 0 ] && export TMOUT=180

# ^d must be pressed twice to exit shell
export IGNOREEOF=1

set -o notify          # Don't wait for job termination notification

# ulimit settings are per-process, 'man bash', not 'man ulimit'
ulimit -c 0     # create no core files
ulimit -m 500000

# Tab (readline) completion settings
set completion-ignore-case on # Set tab-completion to be case-insensitive
set match-hidden-files on
set show-all-if-ambiguous on # Show all tab-completed matches
set show-all-symlinked-directories on # Set all symlinked-directories to be shown

# fixtty : reset TTY after user cat'ed a binary file
fixtty () {
    stty sane
    reset
}

export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export EDITOR='emacsclient -a ""'
export PAGER="/usr/bin/most -s"
export DISPLAY=:0
#http://bash.blogsky.com/1392/09/13/post-213/%D9%BE%DB%8C%D8%B4%E2%80%8C%DA%AF%DB%8C%D8%B1%DB%8C-%D8%A7%D8%B2-%D8%B1%D9%88%D9%86%D9%88%DB%8C%D8%B3%DB%8C-%D9%86%D8%A7%D8%AE%D9%88%D8%A7%D8%B3%D8%AA%D9%87-%D9%81%D8%A7%DB%8C%D9%84
set -o noclobber

#stops ctrl+d from logging me out
#set -o ignoreeof

# Some useful functions
# Also see http://gotux.net/arch-linux/custom-bash-commands-functions/

# https://wiki.archlinux.org/index.php/Proxy_settings
assignProxy(){
    PROXY_ENV="http_proxy ftp_proxy https_proxy all_proxy no_proxy HTTP_PROXY HTTPS_PROXY FTP_PROXY NO_PROXY ALL_PROXY"
    for envar in $PROXY_ENV
    do
        export $envar=$1
    done
}

proxyoff(){
    assignProxy "" # This is what 'unset' does.
}

proxyon(){
    # user=YourUserName
    # read -p "Password: " -s pass &&  echo -e " "
    proxy_value="http://921113616:8681@192.168.200.254:8080"
    assignProxy $proxy_value
}


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

mcd ()
{
    mkdir "$@" && cd "${!#}"
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
complete -F _root_command s

iopri() {
    # 0 for none, 1 for realtime, 2 for best-effort, 3 for idle
    pgrep -d ' ' $1 | sudo xargs ionice -c $2 -p
    pgrep -d ' ' $1 | sudo xargs ionice -p
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

# Runs configure make and make install with dumb error handling
function install_source {
    if [[ $# != 1 ]]; then
        echo "Using $PWD as source location."
    elif [[ $# == 2 ]]; then
        cd $2
    fi

    if [[ ! -x ./configure ]]; then
        echo "Could not find configure script in $PWD!"
        return 1
    fi

    ./configure
    if [[ $? != 0 ]]; then
        echo -e "\nA problem occured with the automated configuration.\n"
        return 1
    fi

    make
    if [[ $? != 0 ]]; then
        echo -e "\nA problem happened while making this program.\n"
        return 1
    fi

    make install
    if [[ $? != 0 ]]; then
        echo -e "\nA problem happened while installing the program.\n"
        return 1
    fi
}

# Enquote: surround lines with quotes (useful in pipes) - from mervTormel
enquote () { /usr/bin/sed 's/^/"/;s/$/"/' ; }

# processes
function p_cpu()
{
    ps -e -o pcpu,cpu,nice,state,cputime,args --sort pcpu \
        | sed '/^ 0.0 /d' | pr -TW$COLUMNS
}
function p_mem()
{
    ps -e -orss=,args= | sort -b -k1,1n | pr -TW$COLUMNS
}
function p_user() { ps aux | grep "^$USER" | pr -TW$COLUMNS; }

# mkmine - recursively change ownership to $USER:$USER
# usage:  mkmine, or
#         mkmine <filename | dirname>
function mkmine() { sudo chown -R ${USER}:${USER} ${1:-.}; }

# sanitize - set file/directory owner and permissions to normal values (644/755)
# usage: sanitize <file>
sanitize()
{
  chmod -R u=rwX,go=rX "$@"
  chown -R ${USER}:users "$@"
}


# nohup - run command detached from terminal and without output
# usage: nh <command>
nh()
{
  nohup "$@" &>/dev/null &
}

# Create a data URL from a file
function dataurl() {
        local mimeType=$(file -b --mime-type "$1")
        if [[ $mimeType == text/* ]]; then
                mimeType="${mimeType};charset=utf-8"
        fi
        echo "data:${mimeType};base64,$(openssl base64 -in "$1" | tr -d '\n')"
}

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
