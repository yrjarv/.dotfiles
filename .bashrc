#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
alias grep='grep --color=auto'
alias ll='ls -l'
alias la='ls -a'
alias lla='ls -la'
alias uio='ssh yrjarv@login.uio.no'
alias ifi='ssh yrjarv@login.ifi.uio.no'
alias home='cd ~ && clear'
alias cybssh='ssh -L 3307:localhost:3306 -i ~/.ssh/dbtunnel dbtunnel@158.39.200.46 -N'
alias cybprisma='cd ~/internsystem-v2 && npx prisma generate'
alias cybdev='cd ~/internsystem-v2 && npm run dev'

PS1='[\u@\h \W]\$ '
