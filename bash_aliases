# https://github.com/joejag/dotfiles/blob/master/bash/aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto -Fh'
    alias l='ls --color=auto -Fh'
    alias lr='ls --color=auto -FhR'     # recursive ls
    alias la='ls --color=auto -Fah'
    alias ll='ls --color=auto -Fahl'
    alias lx='ls --color=auto -lXB' # Sort by extension
    alias lk='ls --color=auto -lSr' # Sort by size (small to big)
    alias lc='ls --color=auto -ltcr' # Sort by change time (old to new)
    alias lu='ls --color=auto -ltur' # Sort by change time (new to old)
    alias lt='ls --color=auto -ltr' # Sort by date (old to new)
    alias l.='ls --color=auto -d .* --color=auto' # Show hidden files
    alias dir='dir --color=auto'
    alias vdir='vdir --color=auto'
    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi
# show most commonly used commands
alias profileme="history | awk '{print \$2}' | awk 'BEGIN{FS=\"|\"}{print \$1}' | sort | uniq -c | sort -n | tail -n 20 | sort -nr"
alias ducurr='du -cksh *' # file sizes for current directory
alias reload='. ~/.bashrc'  # reload
alias locate="locate -eiA"
alias du="du -hc"
alias du1='du --max-depth=1'
alias df="df -Th"
alias ln='ln -s'
alias more='most'
alias less='most'
alias axel='axel -av'
alias wget='wget --tries=40 --read-timeout=20 '
alias cp="cp -iv"      # interactive, verbose, Sorry for that ;)
alias rm="rm -iI --preserve-root"      # do not delete / or prompt if deleting more than 3 files at a time, Shame on me :)
alias mv="mv -iv"       # interactive, verbose 
alias x="exit"
alias ..='cd ..'
alias ...='cd ../..'
alias h='history' # Bash history
alias hs='history | grep $1'
alias j='jobs -l' # Current running jobs
alias mkdir='mkdir -pv' # Create parent directories on demand
alias diff='colordiff' # install  colordiff package :)
alias mount='mount |column -t'
alias chown='chown --preserve-root' # Parenting changing perms on / #
alias chmod='chmod --preserve-root' # Parenting changing perms on / #
alias chgrp='chgrp --preserve-root' # Parenting changing perms on / #
alias psx="ps -auxw ¦ grep $1"
alias path='echo -e ${PATH//:/\\n}'
# pacman aliases (if necessary, replace 'pacman' with your favorite AUR helper and adapt the commands accordingly)
alias pac="sudo /usr/bin/pacman -S"		# default action	- install one or more packages
alias pacu="/usr/bin/pacman -Syu"		# '[u]pdate'		- upgrade all packages to their newest version
alias pacr="sudo /usr/bin/pacman -Rs"		# '[r]emove'		- uninstall one or more packages
alias pacs="/usr/bin/pacman -Ss"		# '[s]earch'		- search for a package using one or more keywords
alias paci="/usr/bin/pacman -Si"		# '[i]nfo'		- show information about a package
alias paclo="/usr/bin/pacman -Qdt"		# '[l]ist [o]rphans'	- list all packages which are orphaned
alias pacc="sudo /usr/bin/pacman -Scc"		# '[c]lean cache'	- delete all not currently installed package files
alias paclf="/usr/bin/pacman -Ql"		# '[l]ist [f]iles'	- list all files installed by a given package
alias pacof="/usr/bin/pacman -Qo"               # '[o]wn [f]iles'       - Search for packages that own the specified file(s)
alias pacexpl="/usr/bin/pacman -D --asexp"	# 'mark as [expl]icit'	- mark one or more packages as explicitly installed 
alias pacimpl="/usr/bin/pacman -D --asdep"	# 'mark as [impl]icit'	- mark one or more packages as non explicitly installed
                                                # '[r]emove [o]rphans' - recursively remove ALL orphaned packages
alias pacro="/usr/bin/pacman -Qtdq > /dev/null && sudo /usr/bin/pacman -Rs \$(/usr/bin/pacman -Qtdq | sed -e ':a;N;$!ba;s/\n/ /g')"

# incase I need to type yaourt..
alias yoaurt='yaourt'
alias yuaort='yaourt'
alias youart='yaourt'
alias yauort='yaourt'
alias yuoart='yaourt'

# These are just for personal use, you can remove them.
alias m='mfind'
alias netstart="sudo netcfg MrGee"
alias unlock="sudo truecrypt /run/media/mrgee/f/data /home/mrgee/tmp/"
alias lock="sudo truecrypt -d"