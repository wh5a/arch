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

# Whenever displaying the prompt, write the previous line to disk;
# If this is an xterm set the title to user@host:dir
PROMPT_COMMAND='history -a; echo -ne "\033]0;${USER}@${HOSTNAME}: ${PWD/$HOME/~}\007"'

# case "$TERM" in
# xterm*|rxvt*)
#     PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME}: ${PWD/$HOME/~}\007"'
#     ;;
# *)
#     ;;
# esac

# set a fancy prompt
PS1='\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '

# colored manpages
export LESS_TERMCAP_mb=$'\E[01;31m'
export LESS_TERMCAP_md=$'\E[01;31m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'                           
export LESS_TERMCAP_so=$'\E[01;44;33m'                                 
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01;32m'

# CodeSurfer
export PATH=~/codesurfer-2.1p1/csurf/bin/:$PATH
export LM_LICENSE_FILE=~/codesurfer-2.1p1/csurf/bin/cs-license.dat

# Emacs-daemon
pgrep -u wh5a emacs > /dev/null
if [ $? -ne 0 ]; then
  emacs -daemon
fi

export EDITOR="emacsclient -c -a emacs"
