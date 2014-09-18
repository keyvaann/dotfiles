# -*- mode: sh; -*-
#
# Executes commands at the start of an interactive session.
#
# Authors:
#   Sorin Ionescu <sorin.ionescu@gmail.com>
#

eval "$(fasd --init posix-alias zsh-hook)"

# Source Prezto.
if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi

# Customize to your needs...

source ~/.shell_config.sh

alias di='dirs -v'

alias a='fasd -a'        # any
alias s='fasd -si'       # show / search / select
alias fd='fasd -d'        # directory
alias f='fasd -f'        # file
alias sd='fasd -sid'     # interactive directory selection
alias sf='fasd -sif'     # interactive file selection
alias z='fasd_cd -d'     # cd, same functionality as j in autojump
alias zz='fasd_cd -d -i' # cd with interactive selection

bindkey ';5C' forward-word
bindkey ';5D' backward-word

hash -d torrent="/run/media/mrgee/d/torrent/downloads"
hash -d music="/run/media/mrgee/e"
hash -d git="/home/mrgee/git"

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

bindkey "^X^S" prepend-sudo

# http://chneukirchen.org/blog/archive/2011/02/10-more-zsh-tricks-you-may-not-know.html
# Complete with words in the history (like Emacs dabbrev) with M-/, M-,
zstyle ':completion:history-words:*' list no
zstyle ':completion:history-words:*' menu yes
zstyle ':completion:history-words:*' remove-all-dups yes
bindkey "\e/" _history-complete-older
bindkey "\e," _history-complete-newer

# Color output (auto set to 'no' on dumb terminals).
zstyle ':prezto:*:*' color 'yes'

# Set the Prezto modules to load (browse modules).
# The order matters.
zstyle ':prezto:load' pmodule \
  'environment' \
  'terminal' \
  'editor' \
  'history' \
  'directory' \
  'spectrum' \
  'utility' \
  'completion' \
  'prompt'

# Set the key mapping style to 'emacs' or 'vi'.
zstyle ':prezto:module:editor' key-bindings 'emacs'

# Set the prompt theme to load.
# Setting it to 'random' loads a random theme.
# Auto set to 'off' on dumb terminals.
zstyle ':prezto:module:prompt' theme 'sorin'

# Additional completion definitions for Zsh.
# https://github.com/zsh-users/zsh-completions
fpath=(~/.zsh-completions $fpath)

# zsh anything.el-like widget.
# https://github.com/zsh-users/zaw
source ~/.zaw/zaw.zsh

source ~/sorin_customized
