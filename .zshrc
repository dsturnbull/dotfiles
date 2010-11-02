# interactive shells

setopt HIST_VERIFY
setopt APPEND_HISTORY
setopt INC_APPEND_HISTORY
setopt SHARE_HISTORY
setopt EXTENDED_HISTORY
setopt HIST_IGNORE_DUPS
setopt prompt_subst
setopt NO_BEEP
setopt EXTENDED_GLOB
setopt MULTIOS
setopt AUTO_PUSHD

bindkey -e

fpath=($HOME/.zsh $fpath)

autoload -U compinit; compinit
autoload -U colors; colors
autoload -U zgitinit; zgitinit
autoload -U git_status

# PS1
nice_orange=$'\e[38;5;208m'
nice_green=$'\e[38;5;154m'
nice_blue=$'\e[38;5;39m'
PROMPT="%j \
[%{$nice_orange%}%n%{$fg[default]%}] \
%{$nice_green%}%m%{$fg_reset%}: \
%{$nice_blue%}%~%{$fg[default]%} \
%# "
RPS1='$(git_status) %*'

# various
# FIXME why don't I need to export these, like the other crap below?
HISTSIZE=10000
SAVEHIST=10000
HISTFILE=~/.zsh_history
WORDCHARS=""
EDITOR=emacs

# bindings
bindkey '^w' kill-region # FIXME need to detect mark-active for ^w to be nice
bindkey '\ed' kill-word
bindkey '\ef' emacs-forward-word
bindkey '\eb' emacs-backward-word

# special dirs
rt=~/src/incite/rt
tbx=~/src/incite/toolbox
smarm=~/src/smarm
ct=~/src/content_type

# fruity colours for less
export LESS_TERMCAP_mb=$'\E[01;31m'
export LESS_TERMCAP_md=$'\E[01;31m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_so=$'\E[01;44;33m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01;32m'

# OS-specific stuff
# Linux
if [ `uname` = "Linux" ]; then
	export JAVA_HOME=/usr/lib/jvm/java-6-sun-1.6.0.13
fi

# Darwin
if [ `uname` = "Darwin" ]; then
	export JAVA_HOME=/System/Library/Frameworks/JavaVM.framework/Home
	export ARCHFLAGS="-arch x86_64"
fi

# make rxvt prettier
if [ $TERM = "rxvt-unicode" ]; then
  export TERM=xterm-256color
fi

# aliases
alias grin='grin --force-color'
alias less='less -R'
alias new='ls -lath $HOME/Downloads/ | head'

# resty
. $HOME/dotfiles/resty/resty

# ec2 keys
if [ -e $HOME/.ec2/local_keys ]; then
  . $HOME/.ec2/local_keys
fi

# I want ^S please
if test -n $PS1; then
	stty stop undef
fi
