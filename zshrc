export TERM="xterm-256color"
POWERLEVEL9K_MODE='awesome-fontconfig'

# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="robbyrussell"

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder
fastfile_var_prefix="@"
# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(
    # alias-tips
    git
    # colorize                    # Plugin for highlighting file content, colorize
    # django                      # compdef manage.py
    # emacs
    docker
    kubectl
    extract
    common-aliases
    colored-man-pages
    git-extras                  # Completion script for git-extras (http://github.com/visionmedia/git-extras).
    pip
    sudo
    # tmux
    zsh-completions
    ssh-agent
    zsh-autosuggestions
    zaw
    zsh-syntax-highlighting     # Note that zsh-syntax-highlighting must be the last plugin sourced, so make it the last element of the $plugins array.
)
echo "11"
export ZSH_AUTOSUGGEST_USE_ASYNC="true"
source $ZSH/oh-my-zsh.sh
echo "22"

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/dsa_id"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

# exports =======================================
export PYTHONSTARTUP=$HOME/.dotfiles/pythonrc.py
export PATH=~/.bin:$PATH

# Security: close root shells after n seconds of inactivity
[ "$UID" = 0 ] && export TMOUT=180

# automatically log out after 2 hours of inactivity
#export TMOUT=7200

# eval & source =================================

[ -r ~/.dotfiles/shell_aliases.sh ] && source ~/.dotfiles/shell_aliases.sh
[ -r ~/.dotfiles/.shell_functions.sh ] && source ~/.dotfiles/shell_functions.sh

# Safe default permissions
#umask 077

# ulimit settings are per-process, 'man bash', not 'man ulimit'
ulimit -c 0     # create no core files
ulimit -m 500000

# bindkey ';5C' forward-word
# bindkey ';5D' backward-word

# Exit incremental search, retaining the command line but performing no further action. Note that this function is not bound by default and has no effect outside incremental search.
bindkey '\ea' accept-search
# Read a character from the keyboard, and move to the next occurrence of it in the line.
bindkey "^X^F" vi-find-next-char
# Read a character from the keyboard, and move to the previous occurrence of it in the line.
bindkey "^X^B" vi-find-prev-char
# Repeat the last vi-find command.
bindkey "^V" vi-repeat-find
# Exchange the current word with the one before it.
bindkey "^[t" transpose-words
# Expand the current command to its full pathname.
bindkey "^[e" expand-cmd-path
# Reads a key sequence, then prints the function bound to that sequence.
bindkey "^h" describe-key-briefly
# Push the current buffer onto the buffer stack and clear the buffer. Next time the editor starts up, the buffer will be popped off the top of the buffer stack and loaded into the editing buffer.
bindkey "^Q" push-line-or-edit
# Set the mark at the cursor position. If called with a negative prefix argument, do not set the mark but deactivate the region so that it is no longer highlighted (it is still usable for other purposes). Otherwise the region is marked as active.
bindkey "^ " set-mark-command
# Quote the region from the cursor to the mark.
bindkey "^['" quote-region
# Push the buffer onto the buffer stack, and execute the command ‘which-command cmd’. where cmd is the current command. which-command is normally aliased to whence.
bindkey "^[?" which-command
# Push the buffer onto the buffer stack, and execute the command ‘run-help cmd’, where cmd is the current command. run-help is normally aliased to man.
bindkey "^[H" run-help

bindkey "^X^S" sudo-command-line

# http://chneukirchen.org/blog/archive/2011/02/10-more-zsh-tricks-you-may-not-know.html
# Complete with words in the history (like Emacs dabbrev) with M-/, M-,
# zstyle ':completion:history-words:*' list no
# zstyle ':completion:history-words:*' menu yes
# zstyle ':completion:history-words:*' remove-all-dups yes
# bindkey "\e/" _history-complete-older
# bindkey "\e," _history-complete-newer

# zsh anything.el-like widget.
# https://github.com/zsh-users/zaw
bindkey '^R' zaw-history
bindkey '^Q' zaw-ssh-hosts
zstyle ':filter-select:highlight' matched fg=yellow,standout
zstyle ':filter-select' max-lines 20 # use 10 lines for filter-select
# zstyle ':filter-select' max-lines -10 # use $LINES - 10 for filter-select
zstyle ':filter-select' rotate-list yes # enable rotation for filter-select
zstyle ':filter-select' case-insensitive yes # enable case-insensitive search
zstyle ':filter-select' extended-search yes # see below

export WORKON_HOME=~/.virtualenv
mkdir -p $WORKON_HOME
source /usr/local/bin/virtualenvwrapper.sh

LANG="en_US.utf8"
LC_COLLATE="en_US.utf8"
LC_CTYPE="en_US.utf8"
LC_MESSAGES="en_US.utf8"
LC_MONETARY="en_US.utf8"
LC_NUMERIC="en_US.utf8"
LC_TIME="en_US.utf8"
LC_ALL="en_US.utf8"
LANGUAGE="en_US.utf8"

alias ssh=sshrc
echo "33"

bindkey "${terminfo[kcuu1]}" up-line-or-history
bindkey "${terminfo[kcud1]}" down-line-or-history
bindkey "^[/" insert-last-word

alias vim=nvim
export GOROOT=/usr/local/go
export PATH=$PATH:$GOROOT/bin:/home/k1/src/golang/bin
export GOPATH=~/src/golang


autoload -U +X bashcompinit && bashcompinit
complete -o nospace -C /home/k1/.bin/mc mc
