# Some useful functions
# Also see http://gotux.net/arch-linux/custom-bash-commands-functions/

# Do sudo, or sudo the last command if no argument given
s() {
    if [[ $# == 0 ]]; then
        sudo $(history -p '!!')
    else
        sudo "$@"
    fi
}
# complete -F _root_command s

# fixtty : reset TTY after user cat'ed a binary file
fixtty () {
    stty sane
    reset
}

# mkdir and ls in one
mcd ()
{
    mkdir -p "$@" && cd "${!#}"
}

# Cd and ls in one
cl() {
    if [ -d "$1" ]; then
        cd "$1"
        ls
        else
        echo "shell: cl: '$1': Directory not found"
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

iopri() {
    # 0 for none, 1 for realtime, 2 for best-effort, 3 for idle
    pgrep -d ' ' $1 | sudo xargs ionice -c $2 -p
    pgrep -d ' ' $1 | sudo xargs ionice -p
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

    sudo make install
    if [[ $? != 0 ]]; then
        echo -e "\nA problem happened while installing the program.\n"
        return 1
    fi
}

# mkmine - recursively change ownership to $USER:$USER
# usage:  mkmine, or
#         mkmine <filename | dirname>
function mkmine() { sudo chown -R ${USER}:${USER} ${1:-.}; }


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

# Change working dir in shell to last dir in lf on exit (adapted from ranger).
#
# You need to either copy the content of this file to your shell rc file
# (e.g. ~/.bashrc) or source this file directly:
#
#     LFCD="/path/to/lfcd.sh"
#     if [ -f "$LFCD" ]; then
#         source "$LFCD"
#     fi
#
# You may also like to assign a key to this command:
#
#     bind '"\C-o":"lfcd\C-m"'  # bash
#     bindkey -s '^o' 'lfcd\n'  # zsh
#

lfcd () {
    tmp="$(mktemp)"
    lf -last-dir-path="$tmp" "$@"
    #./lfrun
    if [ -f "$tmp" ]; then
        dir="$(cat "$tmp")"
        rm -f "$tmp"
        if [ -d "$dir" ]; then
            if [ "$dir" != "$(pwd)" ]; then
                cd "$dir"
            fi
        fi
    fi
}
