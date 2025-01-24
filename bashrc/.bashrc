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

alias uio='kitten @ set-colors background=#300000 && kitten ssh yrjarv@login.uio.no && kitten @ set-colors background=#000000'
alias ifi='kitten @ set-colors background=#000030 && kitten ssh yrjarv@login.ifi.uio.no && kitten @ set-colors background=#000000'

alias adenin='kitten @ set-colors background=#000030 && kitten ssh yrjarv@adenin.ifi.uio.no && kitten @ set-colors background=#000000'
alias guanin='kitten @ set-colors background=#000030 && kitten ssh yrjarv@guanin.ifi.uio.no && kitten @ set-colors background=#000000'
alias sytosin='kitten @ set-colors background=#000030 && kitten ssh yrjarv@sytosin.ifi.uio.no && kitten @ set-colors background=#000000'
alias tymin='kitten @ set-colors background=#000030 && kitten ssh yrjarv@tymin.ifi.uio.no && kitten @ set-colors background=#000000'

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

alias push='find ~/ -maxdepth 2 -type d -name ".git" ! -path "$HOME/internsystem-v2/.git" -exec sh -c '\''cd "$(dirname "{}")" && echo "In directory: $(pwd)" && git push'\'' \;'
alias pull='find ~/ -maxdepth 2 -type d -name ".git" ! -path "$HOME/internsystem-v2/.git" -exec sh -c '\''cd "$(dirname "{}")" && echo "In directory: $(pwd)" && git pull'\'' \;'

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
