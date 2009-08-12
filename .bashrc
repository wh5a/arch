# Check for an interactive session
#[ -z "$PS1" ] && return

alias ls='ls --color=auto'
alias ll='ls -l'
alias p='sudo pacman'
alias y='yaourt --noconfirm'

# set PATH so it includes user's private bin if it exists
if [ -d ~/bin ] ; then
    PATH=~/bin:"${PATH}"
fi
if [ -d ~/.cabal/bin ] ; then
    PATH=~/.cabal/bin:"${PATH}"
fi

# enable programmable completion features
if [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi

# Make less more friendly
export LESS="-iRsX"

# Make Bash append rather than overwrite the history on disk:
shopt -s histappend
# Whenever displaying the prompt, write the previous line to disk:
PROMPT_COMMAND='history -a'
# Save multiple-line command together
shopt -s cmdhist 
# don't put duplicate lines in the history.
export HISTCONTROL=ignoredups

# Syntax-highlighting pager
alias cless='/usr/share/vim/macros/less.sh'

# Bash 4 features
shopt -s autocd globstar checkjobs
# To be used with autocd
alias ...='../..'
alias ....='../../..'

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME}: ${PWD/$HOME/~}\007"'
    ;;
*)
    ;;
esac

# set a fancy prompt
PS1='\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
