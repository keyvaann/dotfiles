# -*- mode: sh; -*-
# https://github.com/joejag/dotfiles/blob/master/bash/aliases
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
alias iotop='sudo iotop -Pkoa'
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
alias pss='ps axfo pid,euser,etime,%cpu,%mem,wchan:20,args'
alias ka='killall -v'
alias j='jobs -l' # Current running jobs
alias diff='colordiff' # install  colordiff package :)
alias showmount='mount | column -t'
alias path='echo -e ${PATH//:/\\n}'

# Better copy
alias rsync-copy="rsync --verbose --progress --human-readable --compress --archive --hard-links --partial"
alias rsync-move="rsync --verbose --progress --human-readable --compress --archive --hard-links --partial --remove-source-files"
alias rsync-update="rsync --verbose --progress --human-readable --compress --archive --hard-links --partial --update"
alias rsync-synchronize="rsync --verbose --progress --human-readable --compress --archive --hard-links --partial --update --delete-after"
