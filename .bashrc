
# Check for an interactive session
[ -z "$PS1" ] && return

alias ls='ls --color=auto'
PS1='[\u@\h \W]\$ '
alias p='sudo pacman'
alias y='yaourt --noconfirm'
export PATH=~/bin:$PATH
