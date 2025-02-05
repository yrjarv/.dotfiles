#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
alias grep='grep --color=auto'

if [[ $(hostname) == *uio* ]]; then
    alias ll='ls -l'
else
    alias ll='eza -lghT --git-repos --git -L=1 --icons --hyperlink'
    alias tree='ll -L=2 -I uio'
    alias cat='bat --style=plain'
fi

alias lla='ll -a -I .git'
alias la='ls -a'

alias treea='tree -a -I .git'
alias treeag='tree -a'

alias gduroot='gdu / --ignore-dirs "/home/y/virtualbox,/home/y/uio,/mnt,/dev,/run"'

function uio() {
    kitten @ set-colors background=#300000
    local hostname
    if [[ -z $1 ]]; then
        hostname="login"
    else
        hostname=$1
    fi
    
    ssh -X yrjarv@"$hostname".uio.no
    
    kitten @ set-colors background=#000000
}

function ifi() {
    kitten @ set-colors background=#000030
    ssh -X yrjarv@"$1".ifi.uio.no
    kitten @ set-colors background=#000000
}

function uiojump() {
    kitten @ set-colors background=#003000
    local jumpname
    local hostname
    if [[ -z $1 ]]; then
	jumpname="login"
	hostname="login.ifi"
    else
	jumpname=$1
	if [[ -z $2 ]]; then
		hostname="login.ifi"
	else
	    hostname=$2
	fi
    fi
    ssh -J yrjarv@"$jumpname".uio.no yrjarv@"$hostname".uio.no
    kitten @ set-colors background=#000000
}

function uio-sftp() {
    local hostname
    if [[ -z $1 ]]; then
	sftp yrjarv@sftp.uio.no
    else
	sftp yrjarv@sftp"$1".uio.no
    fi

}

alias cybssh='ssh -L 3307:localhost:3306 -i ~/.ssh/dbtunnel dbtunnel@158.39.200.46 -N'
alias cybprisma='cd ~/internsystem-v2 && npx prisma generate'
alias cybdev='cd ~/internsystem-v2 && npm run dev'

alias ida='/opt/ida-free-pc-9.0/ida'

alias eduroam='sudo systemctl stop NetworkManager ; sudo systemctl stop wpa_supplicant ; sudo wpa_supplicant -B -i wlan0 -c ~/.config/cat_installer/cat_installer.conf'

alias push='find ./ -maxdepth 2 -type d -name ".git" ! -path "*/internsystem-v2/.git" -exec sh -c '\''cd "$(dirname "{}")" && echo "In directory: $(pwd)" && git push'\'' \;'
alias pull='find ./ -maxdepth 2 -type d -name ".git" ! -path "*/internsystem-v2/.git" -exec sh -c '\''cd "$(dirname "{}")" && echo "In directory: $(pwd)" && git pull'\'' \;'
alias status='find ./ -maxdepth 2 -type d -name ".git" ! -path "*/internsystem-v2/.git" -exec sh -c '\''cd "$(dirname "{}")" && echo "In directory: $(pwd)" && git status'\'' \;'

alias endday='cd && push && shutdown now'

export EDITOR="/usr/bin/vim"

PS1='[\u@\h \W]\$ '

