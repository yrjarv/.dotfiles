#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
alias grep='grep --color=auto'

alias ll='eza -lghT --git-repos --git -L=1 --icons --hyperlink'
alias lla='ll -a -I .git'
alias la='ls -a'

alias tree='eza -lghT -L=2 --git-repos --git --icons --hyperlink'
alias treea='tree -a -I .git'
alias treeag='tree -a'

alias cat='bat'

alias gduroot='gdu / --ignore-dirs "/home/y/virtualbox,/mnt,/proc,/dev,/sys,/run"'

function uio() {
    kitten @ set-colors background=#300000
    local hostname
    if [[ -z $1 ]]; then
        hostname="login"
    else
        hostname=$1
    fi
    kitten ssh -X yrjarv@"$hostname".uio.no
    kitten @ set-colors background=#000000
}

function ifi() {
    kitten @ set-colors background=#000030
    local hostname
    if [[ -z $1 ]]; then
        hostname="login"
    else
        hostname=$1
    fi
    kitten ssh -X yrjarv@"$hostname".ifi.uio.no
    kitten @ set-colors background=#000000
}

ssh() {
    kitten @ set-colors background=#003000
    command kitten ssh "$@"
    kitten @ set-colors background=#000000
}


alias cybssh='ssh -L 3307:localhost:3306 -i ~/.ssh/dbtunnel dbtunnel@158.39.200.46 -N'
alias cybprisma='cd ~/internsystem-v2 && npx prisma generate'
alias cybdev='cd ~/internsystem-v2 && npm run dev'

alias ida='/opt/ida-free-pc-9.0/ida'

alias eduroam='sudo systemctl stop NetworkManager ; sudo systemctl stop wpa_supplicant ; sudo wpa_supplicant -B -i wlan0 -c ~/.config/cat_installer/cat_installer.conf'

alias push='find ./ -maxdepth 2 -type d -name ".git" ! -path "*/internsystem-v2/.git" -exec sh -c '\''cd "$(dirname "{}")" && echo "In directory: $(pwd)" && git push'\'' \;'
alias pull='find ./ -maxdepth 2 -type d -name ".git" ! -path "*/internsystem-v2/.git" -exec sh -c '\''cd "$(dirname "{}")" && echo "In directory: $(pwd)" && git pull'\'' \;'

alias endday='push && shutdown now'

alias 1010='cd ~/in1010'
alias 1030='cd ~/in1030'
alias 1150='cd ~/in1150'
alias 2140='cd ~/in2140'
alias 5290='cd ~/in5290'
alias dotfiles='cd ~/.dotfiles'
alias intern='cd ~/internsystem-v2'
alias down='cd ~/Downloads'
alias priv='cd ~/private-filer'


export EDITOR="/usr/bin/vim"

PS1='[\u@\h \W]\$ '
