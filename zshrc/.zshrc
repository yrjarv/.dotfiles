[[ $- != *i* ]] && return

# Binaries for systems without sudo permission
export PATH=~/.binaries/bin:$PATH

# Default aliases
alias ls='ls --color=auto'
alias grep='grep --color=auto'

# Aliases that won't work on UiO servers
alias ll='eza -lghT --git-repos --git -L=1 --icons --hyperlink\
	--group-directories-first'

# Important aliases requiring other programs to be installed
alias tree='ll -L=10'
alias cat='bat --style=plain --paging=always'
alias codecat='bat --style=plain -n'
eval $(thefuck --alias fuck)
eval $(thefuck --alias faen)

# Use bat as man pager
export MANPAGER="sh -c 'col -bx | bat -l man -p'"
export MANROFFOPT="-c"

# Aliases to quickly change directories
alias 2010='cd ~/in2010'
alias 2090='cd ~/in2090'
alias 3210='cd ~/in3210'
alias 5290='cd ~/in5290'
alias web='cd ~/website/src'
alias ..='cd ..'

# Listing directories
alias lla='ll -a'
alias la='ls -a'
alias treea='tree -a -I .git'
alias treeag='tree -a'
alias llread="lla --no-symlinks | grep '^.\{4\}r'" # To check read permissions

# Nice to have
alias calendar='cal -wym' # Calendar for the whole year
alias gduroot='gdu / --ignore-dirs\
	"/home/y/virtualbox,/mnt,/dev,/run,/usr/share/wordlists"' # Ignore some
	# large directories in GDU
alias 2090-db='PAGER="bat --style=plain --paging=always" psql -h dbpg-ifi-kurs03 -U yrjarv -d'
alias eduroam-sucks='nmcli connection down eduroam && nmcli connection up\
	eduroam'
alias pdf='termpdf -kitty'

# Neovim
alias n='nvim'
alias nconf='cd ~/.nvim/config/.config/nvim'

# Programs
alias todo='~/todo/todo.py' # For my own todolist script
alias ttr='~/ttr/ttr' # For my own todolist script
alias ida='/opt/ida-free-pc*/ida' # This isn't in PATH and I am too lazy to fix
							      # it
alias py='python3' # python3 takes too long to type
alias k='kubectl' # kubectl takes too long to write
alias icat='kitten icat' # To show images in Kitty

# SFTP into UiO file server
# There are two servers, sftp1.uio.no and stfp2.uio.no. With this function, I
# can choose which one to connect to - or be assigned one randomly through
# sftp.uio.no
function uio-sftp() {
    if [[ -z $1 ]]; then
	sftp yrjarv@sftp.uio.no
    else
	sftp yrjarv@sftp"$1".uio.no
    fi
}

# Bulk run commands on all IFI servers
function ifi-control-panel() {
    local session_id=$(cat /proc/sys/kernel/random/uuid)
    tmux new-session -d -s "$session_id" "ssh guanin -t -i $@"
    tmux split-window -h "ssh sytosin -t -i $@"
    tmux split-window -v "ssh adenin -t -i $@"
    tmux select-pane -t 0
    tmux split-window -v "ssh tymin -t -i $@"
    tmux select-layout tiled
    tmux attach-session -t "$session_id"
}

# These aliases perform actions in all git repos directly inside the current
# directory
alias push='find ~/ -maxdepth 2 -type d -name ".git" -exec sh -c '\''cd "$(dirname "{}")" && echo "In directory: $(pwd)" && git push'\'' \;'
alias pull='find ./ -maxdepth 2 -type d -name ".git" -exec sh -c '\''cd "$(dirname "{}")" && echo "In directory: $(pwd)" && git pull'\'' \;'

function status() {
  find ./ -maxdepth 2 -type d -name ".git" -exec sh -c '
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

# Exported variables
export EDITOR="/usr/bin/nvim" # Instead of nano
export MANSECT="2:3:1:8:5:4:7:6:9:3P" # Prevents all results from being from 3P

# Prompt (`(HH:MM:SS) [user@hostname directory]`)
PROMPT='(%*) [%n@%m %~]$ '

# Make the computer/server cache a lot of files when a new terminal is opened
(tree &) > /dev/null

# Built-in completion
autoload compinit && compinit
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Za-z}'

# Dotglob - allows autocomplete and * to match .files
setopt dotglob

# Sourcing plugins not available on UiO servers
if [[ $(cat /etc/hostname) != *uio* ]]; then
	# Syntax highlighting
	source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
	# Completion
	source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh
	# Vim mode
	source /usr/share/zsh/plugins/zsh-vi-mode/zsh-vi-mode.plugin.zsh
fi

# Fix bck-i-search
HISTFILE=~/.zsh_history
HISTSIZE=100000
SAVEHIST=100000
setopt INC_APPEND_HISTORY
setopt HIST_IGNORE_DUPS

# Keybinds
bindkey '^[[3;5~' kill-word
bindkey '^H' backward-kill-word
bindkey '^[[3~' delete-char
bindkey '^[[1;5C' forward-word
bindkey '^[[1;5D' backward-word
bindkey "\e[1~" beginning-of-line
bindkey "\e[4~" end-of-line
bindkey "\e[H" beginning-of-line
bindkey "\e[F" end-of-line
