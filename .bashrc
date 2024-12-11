#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
alias grep='grep --color=auto'
alias ll='eza -lghT -L=1 --git-repos --git --icons'
alias la='ls -a'
alias lla='ll -a'
alias cat='bat'
alias tree='eza -lghT -L=5 --git-repos --git --icons'
alias uio='ssh yrjarv@login.uio.no'
alias ifi='ssh yrjarv@login.ifi.uio.no'
alias cybssh='ssh -L 3307:localhost:3306 -i ~/.ssh/dbtunnel dbtunnel@158.39.200.46 -N'
alias cybprisma='cd ~/internsystem-v2 && npx prisma generate'
alias cybdev='cd ~/internsystem-v2 && npm run dev'
alias ida='/usr/bin/idafree-8.4/ida64'

export EDITOR="/usr/bin/vim"

PS1='[\u@\h \W]\$ '
