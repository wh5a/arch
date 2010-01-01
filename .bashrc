# Check for an interactive session
#[ -z "$PS1" ] && return

alias ls='ls --color=auto'
alias ll='ls -l'
# Get reflector (AUR) for the latest mirrors
#alias p='sudo powerpill'
alias p='sudo pacman'
alias y='yaourt' # --noconfirm'
alias skype='LD_PRELOAD=/usr/lib/libv4l/v4l1compat.so /opt/skype/skype'

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
alias cless='/usr/share/vim/vim72/macros/less.sh'

# Bash 4 features
shopt -s autocd globstar checkjobs
# To be used with autocd
alias ...='../..'
alias ....='../../..'

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# Whenever displaying the prompt, write the previous line to disk;
PROMPT_COMMAND='history -a'

case "$TERM" in
    xterm*|rxvt*)
        # If this is an xterm set the title to user@host:dir
        PROMPT_COMMAND=$PROMPT_COMMAND';echo -ne "\033]0;${USER}@${HOSTNAME}: ${PWD/$HOME/~}\007"'
        # set a fancy prompt
        PS1='\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
        ;;
    *)
        ;;
esac

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
# pgrep -u wh5a emacs > /dev/null
# if [ $? -ne 0 ]; then
#   emacs -daemon
# fi
pgrep -u wh5a emacs > /dev/null || LANG=zh_CN.UTF-8 emacs -daemon

export EDITOR="emacsclient -c -a emacs"

export GREP_OPTIONS='--color=auto --exclude-dir=_darcs'

export OOO_FORCE_DESKTOP=kde

export PATH=~/cil/bin/:$PATH

# ocaml-batteries
alias bocaml='ocamlfind batteries/ocaml'
alias bocamlc='ocamlfind batteries/ocamlc'
alias bocamlbuild='ocamlfind batteries/ocamlbuild'
alias bocamlopt='ocamlfind batteries/ocamlopt'

export PATH=~/paktahn:$PATH

alias ddc='~/ddc-head/bin/ddc -basedir ~/ddc-head'

# http://blog.nelhage.com/archives/27
stty -ixon
