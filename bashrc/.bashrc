#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
alias grep='grep --color=auto'

alias ll='eza -lghT -L=1 --git-repos --git --icons --hyperlink'
alias la='ls -a'
alias lla='ll -a -I .git'
alias llag='ll -a'

alias cat='bat'

alias tree='eza -lghT -L=2 --git-repos --git --icons --hyperlink'
alias treea='tree -a -I .git'
alias treeag='tree -a'

alias uio='ssh yrjarv@login.uio.no'
alias ifi='ssh yrjarv@login.ifi.uio.no'

alias cybssh='ssh -L 3307:localhost:3306 -i ~/.ssh/dbtunnel dbtunnel@158.39.200.46 -N'
alias cybprisma='cd ~/internsystem-v2 && npx prisma generate'
alias cybdev='cd ~/internsystem-v2 && npm run dev'

alias ida='/opt/ida-free-pc-9.0/ida'

alias eduroam='sudo systemctl stop NetworkManager ; sudo systemctl stop wpa_supplicant ; sudo wpa_supplicant -B -i wlan0 -c ~/.config/cat_installer/cat_installer.conf'

alias push='find ~/ -type d -name ".git" ! -path "$HOME/internsystem-v2/.git" -exec sh -c '\''cd "$(dirname "{}")" && echo "In directory: $(pwd)" && git push'\'' \;'
alias pull='find ~/ -type d -name ".git" ! -path "$HOME/internsystem-v2/.git" -exec sh -c '\''cd "$(dirname "{}")" && echo "In directory: $(pwd)" && git pull'\'' \;'



export EDITOR="/usr/bin/vim"

PS1='[\u@\h \W]\$ '
