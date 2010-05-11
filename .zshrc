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
fg_lblue=$'\e[1;34m'
PROMPT="%j \
[%{$fg[yellow]%}%n%{$fg[default]%}] \
%{$fg[green]%}%m: %{$fg[default]%}\
%{$fg[cyan]%}%~%{$fg[default]%} \
%# "
RPS1='$(git_status) %*'

# various
HISTSIZE=10000
SAVEHIST=10000
HISTFILE=~/.zsh_history
WORDCHARS=${}

# path
PATH=/opt/local/bin:/opt/local/sbin:/usr/local/bin:/usr/local/sbin:$PATH
PATH=/opt/local/libexec/git-core:$PATH

# bindings
bindkey '^w' kill-region # FIXME need to detect mark-active for ^w to be nice

# special dirs
rt=~/src/incite/rt
tbx=~/src/incite/toolbox
smarm=~/src/smarm
ct=~/src/content_type
