# interactive shells

setopt AUTO_PUSHD
setopt PUSHD_IGNORE_DUPS

setopt AUTO_LIST
setopt AUTO_MENU
setopt AUTO_PARAM_KEYS
setopt AUTO_PARAM_SLASH
setopt LIST_TYPES

setopt HIST_VERIFY
setopt APPEND_HISTORY
setopt INC_APPEND_HISTORY
setopt EXTENDED_HISTORY
setopt HIST_IGNORE_DUPS
setopt PROMPT_SUBST
setopt NO_BEEP
setopt EXTENDED_GLOB
setopt MULTIOS
setopt CLOBBER

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
PROMPT="%{$nice_green%}%m%{$fg_reset%} %{$nice_blue%}%~%{$fg[default]%} %# "
RPS1='$(git_status) (%j)'

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
bindkey '\e[A' history-search-backward
bindkey '\e[B' history-search-forward

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
  alias ls='ls --color=auto'
fi

# Darwin
if [ `uname` = "Darwin" ]; then
  export JAVA_HOME=/System/Library/Frameworks/JavaVM.framework/Home
  #export ARCHFLAGS="-arch x86_64"
  alias ls='ls -G'
  alias mvim='mvim --remote'
fi

# make EVERYTHING prettier
export TERM=xterm-256color

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

# just doit
. `brew --prefix`/etc/autojump
. ~/.zsh/cap_completion
. ~/.zsh/osx.plugin.zsh
. ~/.zsh/title

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*
if [[ -f "$HOME/.amazon_keys" ]]; then source "$HOME/.amazon_keys"; fi

export EC2_PRIVATE_KEY="$(/bin/ls $HOME/.ec2/pk-*.pem)"
export EC2_CERT="$(/bin/ls $HOME/.ec2/cert-*.pem)"
export EC2_HOME="/usr/local/Cellar/ec2-api-tools/1.4.4.1/jars"

