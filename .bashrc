. /etc/profile

# Check for an interactive session
#[ -z "$PS1" ] && return

alias ls='ls --color=auto -F'
alias ll='ls -l'
alias p='sudo pacman'
alias y='yaourt' # --noconfirm'
alias octave='octave -q'
alias grep='grep --color=auto --exclude-dir=_darcs --exclude-dir=\.svn --exclude-dir=\.git --exclude-dir=\.hg'
# Syntax-highlighting pager
alias cless='/usr/share/vim/vim80/macros/less.sh'
# Completely wipe urxvt buffer
alias cls="echo -ne '\033c'"

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
export LESS="-iFRsX"
export PAGER=less

# Make Bash append rather than overwrite the history on disk:
shopt -s histappend
# Save multiple-line command together
shopt -s cmdhist 
# don't put duplicate lines in the history.
export HISTCONTROL=ignoredups
# Huge history
export HISTSIZE=1000000
# Ignore a few specified commands
export HISTIGNORE=ls:'ls -l':fg:up

# Bash 4 features
shopt -s autocd globstar checkjobs
# To be used with autocd
alias ...='../..'
alias ....='../../..'

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

########### We now use a more advanced script:  http://bbs.archlinux.org/viewtopic.php?id=84386
# case "$TERM" in
#     xterm*|rxvt*)
#         # If this is an xterm set the title to user@host:dir
#         PROMPT_COMMAND=$PROMPT_COMMAND';echo -ne "\033]0;${USER}@${HOSTNAME}: ${PWD/$HOME/~}\007"'
#         # set a fancy prompt
#         PS1='\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
#         ;;
#     *)
#         ;;
# esac
###########
## Although it suggests sourcing it also for root, doing so halts Emacs/Tramp.
source ~/.zer0prompt
zer0prompt
unset zer0prompt

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
#pgrep -u wh5a emacs > /dev/null || LANG=zh_CN.UTF-8 emacs -daemon

# With alternate-editor being empty string, emacsclient knows to start a daemon automatically.
export EDITOR="emacsclient -c -a ''"

export OOO_FORCE_DESKTOP=kde
# Hack to make xdg-open call gnome-open
export GNOME_DESKTOP_SESSION_ID=anything
# Or kde-open:
# export KDE_FULL_SESSION=true

# ocaml-batteries
alias bocaml='ocaml -init ~/.batteriesinit'

# http://blog.nelhage.com/archives/27
stty -ixon

# cope-git (AUR), a command line coloriser, kinda supersedes colorgcc
export PATH=`cope_path`:$PATH

# compleat, a bash completion util, http://github.com/mbrubeck/compleat
if [ -d ~/.cabal/share/compleat-1.0 ]; then
    . ~/.cabal/share/compleat-1.0/compleat_setup
fi

# http://stackoverflow.com/questions/994563/integrate-readlines-kill-ring-and-the-x11-clipboard
# M-u, M-k, M-y are similar to C-u, C-k, C-y, but deal with X primary selection.
# C-u kills back to the beginning, and C-w kills back a word.
_xdiscard() {
    echo -n "${READLINE_LINE:0:$READLINE_POINT}" | xclip
    READLINE_LINE="${READLINE_LINE:$READLINE_POINT}"
    READLINE_POINT=0
}
_xkill() {
    echo -n "${READLINE_LINE:$READLINE_POINT}" | xclip
    READLINE_LINE="${READLINE_LINE:0:$READLINE_POINT}"
}
_xyank() {
    CLIP=$(xclip -o)
    COUNT=`expr length "$CLIP"`
    READLINE_LINE="${READLINE_LINE:0:$READLINE_POINT}${CLIP}${READLINE_LINE:$READLINE_POINT}"
    READLINE_POINT=$(($READLINE_POINT + $COUNT))
}
_xpaste() {
    CLIP=$(xclip -o -selection clipboard)
    COUNT=`expr length "$CLIP"`
    READLINE_LINE="${READLINE_LINE:0:$READLINE_POINT}${CLIP}${READLINE_LINE:$READLINE_POINT}"
    READLINE_POINT=$(($READLINE_POINT + $COUNT))
}
bind -m emacs -x '"\eu": _xdiscard'
bind -m emacs -x '"\ek": _xkill'
bind -m emacs -x '"\ey": _xyank'
# We implement pasting from the clipboard in urxvt instead of bash
#bind -m emacs -x '"\ev": _xpaste'

# Read by chromium-browser.sh, which is usually installed to /usr/bin/chromium
# See chrome/common/chrome_switches.cc for all flags, see chrome/browser/about_flags.cc for about:flags
export CHROMIUM_USER_FLAGS="--enable-experimental-extension-apis --purge-memory-button --allow-scripting-gallery --enable-preconnect --enable-click-to-play --enable-fastback --reload-killed-tabs --enable-in-browser-thumbnailing --enable-tab-groups-context-menu --multi-profiles --ppapi-flash-in-process --enable-web-intents --enable-webui-taskmanager --prerender=auto"
# --enable-vertical-tabs --user-data-dir=/foo/bar --register-pepper-plugins=libpdf.so;application/pdf --show-composited-layer-borders --enable-print-preview --enable-tcp-fastopen --new-tab-page --proxy-pac-url=file:///home/wh5a/proxy

# http://github.com/wh5a/git-achievements
# http://wh5a.github.com/git-achievements/
#export PATH=$PATH:~/git-achievements
#alias git='git-achievements'

# http://github.com/ddollar/git-utils
export PATH=$PATH:~/git-utils

export DARCS_ALWAYS_COLOR=1
export DARCS_DO_COLOR_LINES=1

# http://www.reddit.com/r/programming/comments/bkmtm/spend_a_lot_of_time_cding_around_a_complex/
alias bd='popd > /dev/null'
function cd {
    if [ $# -eq 0 ]; then
        builtin cd
    else
        pushd > /dev/null "$*"
    fi
}

# Don't let SPEC CPU 2006 set the locale
export SPEC_LOCALE_OK=THIS_CAN_BE_ANYTHING
