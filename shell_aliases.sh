# -*- mode: sh; -*-
# https://github.com/joejag/dotfiles/blob/master/bash/aliases
if [ -x /usr/bin/dircolors ]; then
    # test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto --group-directories-first --classify --ignore-backups -h'
    alias l='ls --color=auto --group-directories-first --classify --ignore-backups -h'
    alias lr='ls --color=auto --group-directories-first --classify --ignore-backups -hR'     # recursive ls
    alias la='ls --color=auto --group-directories-first --classify --ignore-backups -ah'
    alias ll='ls --color=auto --group-directories-first --classify --ignore-backups -ahl'
    alias lx='ls --color=auto --group-directories-first --classify --ignore-backups -lX' # Sort by extension
    alias lk='ls --color=auto --group-directories-first --classify --ignore-backups -lSr' # Sort by size (small to big)
    alias lc='ls --color=auto --group-directories-first --classify --ignore-backups -ltcr' # Sort by change time (old to new)
    alias lu='ls --color=auto --group-directories-first --classify --ignore-backups -ltur' # Sort by change time (new to old)
    alias lt='ls --color=auto --group-directories-first --classify --ignore-backups -ltr' # Sort by date (old to new)
    alias l.='ls --color=auto --group-directories-first --classify --ignore-backups -d .*' # Show hidden files
    alias dir='dir --color=auto'
    alias vdir='vdir --color=auto'
    alias grep='grep --color=auto --line-number --with-filename'
    alias fgrep='fgrep --color=auto --line-number --with-filename'
    alias egrep='egrep --color=auto --line-number --with-filename'
fi

# Easier navigation
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias -- -="cd -"

# alias Ps="ps aux|grep --color"
# alias kl="kill"
# alias kla="killall"
# alias f="find . -iname "

# View HTTP traffic
alias sniff="sudo ngrep -d 'enp1s0' -t '^(GET|POST) ' 'tcp and port 80'"
alias httpdump="sudo tcpdump -i enp1s0 -n -s 0 -w - | grep -a -o -E \"Host\: .*|GET \/.*\""
alias netuse='sudo nethogs wlp2s0'
alias dig='dig ANY +noall +answer ' # dig deeper

alias e='~/git/snippets/runemacs.sh'
alias ec='emacsclient -t -a ""'

# show most commonly used commands
alias profileme="history | awk '{print \$2}' | awk 'BEGIN{FS=\"|\"}{print \$1}' | sort | uniq -c | sort -n | tail -n 20 | sort -nr"

# Enable aliases to be sudo’ed
alias sudo='sudo '
alias s='s '

alias ducurr='du -cksh *' # file sizes for current directory
alias pgrep='pgrep -la'
alias ping='ping -DO'
alias reload='. ~/.bashrc'  # reload
alias locate="locate -eiA"
alias du="du -hc"
alias du1='du --max-depth=1'
alias df="df -Th"
alias ln='ln -s'
alias more='most'
alias iotop='sudo iotop -Pkoa'
alias axel='axel -av'
alias errorlog='sudo journalctl -efp 5'
alias alllog='sudo journalctl -ef'

alias cp="cp -iv"      # interactive, verbose, Sorry for that ;)
alias rm="rm -iI --preserve-root"      # do not delete / or prompt if deleting more than 3 files at a time, Shame on me :)
alias mv="mv -iv"       # interactive, verbose
alias chown='chown --preserve-root' # Parenting changing perms on / #
alias chmod='chmod --preserve-root' # Parenting changing perms on / #
alias chgrp='chgrp --preserve-root' # Parenting changing perms on / #
alias mkdir='mkdir -pv' # Create parent directories on demand

alias x="exit"
alias pcat="pygmentize -g"  #  https://coderwall.com/p/o6_ong?&p=6&q=
alias h='history' # Bash history
alias hs='history | grep $1'
alias pss='ps -A -o pid,uname,%cpu,%mem,stat,time,args | grep '
#alias ps='ps x o pid,comm,args,pcpu,size,state' #prints really nice
alias psx="\ps -auxw ¦ grep $1"
alias j='jobs -l' # Current running jobs
alias diff='colordiff' # install  colordiff package :)
alias showmount='mount | column -t'
alias path='echo -e ${PATH//:/\\n}'

# pacman aliases (if necessary, replace 'pacman' with your favorite AUR helper and adapt the commands accordingly)
alias pac="sudo pacman -S"              # default action        - install one or more packages
alias pacu="sudo pacman -Syu"           # '[u]pdate'            - upgrade all packages to their newest version
alias pacr="sudo pacman -Rs"            # '[r]emove'            - uninstall one or more packages
alias pacs="pacman -Ss"                 # '[s]earch'            - search for a package using one or more keywords
alias pacqs="pacman -Qs"                # '[q]uery [s]earch'    - To search for already installed packages
alias paci="pacman -Sii"                # '[i]nfo'              - show information about a package
alias pacqi="pacman -Sii"                # '[q]uery [i]nfo'      - show information about a for locally installed package
alias paclo="pacman -Qdt"               # '[l]ist [o]rphans'    - list all packages which are orphaned
alias pacc="sudo pacman -Scc"           # '[c]lean cache'       - delete all not currently installed package files
alias paclf="pacman -Ql"                # '[l]ist [f]iles'      - list all files installed by a given package
alias pacof="pacman -Qo"                # '[o]wn [f]iles'       - Search for packages that own the specified file(s)
alias pacexpl="pacman -D --asexp"       # 'mark as [expl]icit'  - mark one or more packages as explicitly installed
alias pacimpl="pacman -D --asdep"       # 'mark as [impl]icit'  - mark one or more packages as non explicitly installed
                                        # '[l]ist [e]xplicit'   - Lists explicitly installed Pacman packages.
alias pacle="pacman --query --explicit --info | awk 'BEGIN { FS=\":\" } /^Name/ { printf $2 \":\" } /^Description/ { print $2 } '"
                                        # '[r]emove [o]rphans' - recursively remove ALL orphaned packages
alias pacro="pacman -Qtdq > /dev/null && sudo pacman -Rs \$(pacman -Qtdq)"

# incase I need to type yaourt..
alias yoaurt='yaourt'
alias yuaort='yaourt'
alias youart='yaourt'
alias yauort='yaourt'
alias yuoart='yaourt'

# Better copy
alias rsync-copy="rsync --verbose --progress --human-readable --compress --archive --hard-links --partial"
alias rsync-move="rsync --verbose --progress --human-readable --compress --archive --hard-links --partial --remove-source-files"
alias rsync-update="rsync --verbose --progress --human-readable --compress --archive --hard-links --partial --update"
alias rsync-synchronize="rsync --verbose --progress --human-readable --compress --archive --hard-links --partial --update --delete-after"

# These are just for personal use, you can remove them.
alias m='mfind'
alias unlock="sudo truecrypt /run/media/mrgee/f/data /home/mrgee/.tmp/"
alias lock="sudo truecrypt -d"
alias d='mdic -W'
