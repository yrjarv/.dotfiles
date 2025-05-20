[[ $- != *i* ]] && return

# Default aliases
alias ls='ls --color=auto'
alias grep='grep --color=auto'

# Aliases that won't work on UiO servers
host=$(cat /etc/hostname)
if [[ $host == *uio* || $host == *kali* ]]; then # if on UiO server or Kali VM
    alias ll='ls -lh'
else
    alias ll='eza -lghT --git-repos --git -L=1 --icons --hyperlink --group-directories-first'
    alias tree='ll -L=10'
    alias cat='bat --style=plain --paging=always'
    eval $(thefuck --alias fuck)
    eval $(thefuck --alias faen)
fi

# Aliases to quickly change directories
alias 1010='cd ~/in1010'
alias 1030='cd ~/in1030'
alias 1150='cd ~/in1150'
alias 2140='cd ~/in2140'
alias web='cd ~/website/src'

# Listing directories
alias lla='ll -a'
alias la='ls -a'
alias treea='tree -a -I .git'
alias treeag='tree -a'

# Nice to have
alias calendar='cal -wym'   # Calendar for the whole year
alias gduroot='gdu / --ignore-dirs "/home/y/virtualbox,/mnt,/dev,/run"' # Ignore some large directories in GDU

# Neovim
alias n='nvim'
alias nconf='cd ~/nvim/config/.config/nvim'

# Programs
alias todo='~/todo/todo.py' # For my own todolist script
alias ida='/opt/ida-free-pc*/ida' # For some reason, this isn't in PATH and I am too lazy to fix it
alias emacs='emacs -nw' # To make emacs behave like vim/neovim when in the terminal

# SFTP into UiO file server
# There are two servers, sftp1.uio.no and stfp2.uio.no. With this function, I
# can choose which one to connect to - or be assigned one randomly through
# sftp.uio.no
function uio-sftp() {
    local hostname
    if [[ -z $1 ]]; then
	sftp yrjarv@sftp.uio.no
    else
	sftp yrjarv@sftp"$1".uio.no
    fi
}

# Git
# These aliases perform actions in all git repos directly inside ~, except
# ~/internsystem-v2, because that is a shared project where I want manually do
# any changes to upstream or origin.
alias push='find ./ -maxdepth 2 -type d -name ".git" ! -path "*/internsystem-v2/.git" -exec sh -c '\''cd "$(dirname "{}")" && echo "In directory: $(pwd)" && git push'\'' \;'
alias pull='find ./ -maxdepth 2 -type d -name ".git" ! -path "*/internsystem-v2/.git" -exec sh -c '\''cd "$(dirname "{}")" && echo "In directory: $(pwd)" && git pull'\'' \;'
function status() {
  find ./ -maxdepth 2 -type d -name ".git" ! -path "*/internsystem-v2/.git" -exec sh -c '
    cd "$(dirname "$1")" || exit
    branch_status=$(git status -sb | grep -E "ahead|behind")
    worktree_status=$(git status --short)
    
    if [ -n "$branch_status" ] || [ -n "$worktree_status" ]; then
      echo "$(pwd)"
      git status
      echo "

      "
    fi
  ' sh {} \;
}
alias endday='cd && push && shutdown now'

# Just nice to have, to run a command forever
function forever() {
    while true; do $*; done
}

# Makes keybinds behave more like bash
bindkey -e

# Completion
autoload compinit && compinit

export EDITOR="/usr/bin/nvim" # Instead of nano
export MANSECT="2:3:1:8:5:4:7:6:9:3P" # Prevents Posix syscall manual from being default

# Prompt (`(HH:MM:SS) [user@hostname directory]`)
PROMPT='(%*) [%n@%m %~]$ '
