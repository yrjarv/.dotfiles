#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
alias grep='grep --color=auto'

if [[ $(cat /etc/hostname) == *uio* ]]; then
    alias ll='ls -lh'
else
    alias ll='eza -lghT --git-repos --git -L=1 --icons --hyperlink'
    alias tree='ll -L=10'
    alias cat='bat --style=plain --paging=always'
fi

alias 1010='cd ~/in1010'
alias 1030='cd ~/in1030'
alias 1030o='cd ~/oblig-in1030'
alias 1150='cd ~/in1150'
alias 2140='cd ~/in2140'
alias 2140o='cd ~/oblig-in2140'
alias he='cd ~/oblig-in2140/eksamen/src'

alias lla='ll -a -I .git'
alias la='ls -a'

alias treea='tree -a -I .git'
alias treeag='tree -a'

alias please='sudo'
eval $(thefuck --alias fuck)
eval $(thefuck --alias faen)

alias todo='~/todo/todo.py'

alias gduroot='gdu / --ignore-dirs "/home/y/virtualbox,/home/y/uio,/mnt,/dev,/run"'

function uio-sftp() {
    local hostname
    if [[ -z $1 ]]; then
	sftp yrjarv@sftp.uio.no
    else
	sftp yrjarv@sftp"$1".uio.no
    fi
}

alias cybdev='cd ~/internsystem-v2 && npm run dev'

alias ida='/opt/ida-free-pc-9.0/ida'

alias emacs='emacs -nw'

alias old_eduroam_hack='sudo systemctl stop NetworkManager ; sudo systemctl stop wpa_supplicant ; sudo wpa_supplicant -B -i wlan0 -c ~/.config/cat_installer/cat_installer.conf'

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

source /usr/share/bash-completion/completions/git

export EDITOR="/usr/bin/vim"

export MANSECT="2:3:1:8:5:4:7:6:9:3P"

PS1='[\u@\h \W]\$ '
