# interactive shells

setopt PUSHD_IGNORE_DUPS

setopt AUTO_LIST
setopt LIST_AMBIGUOUS
setopt NO_AUTO_MENU
setopt NO_MENU_COMPLETE

setopt COMPLETE_IN_WORD
setopt AUTO_PARAM_KEYS
setopt AUTO_PARAM_SLASH
setopt AUTO_REMOVE_SLASH

setopt HIST_VERIFY
setopt APPEND_HISTORY
setopt INC_APPEND_HISTORY
setopt EXTENDED_HISTORY
setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_SPACE

setopt PROMPT_SUBST
setopt NO_BEEP
setopt EXTENDED_GLOB
setopt MULTIOS
setopt CLOBBER
setopt NO_FLOW_CONTROL
#setopt RM_STAR_SILENT

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
LISTMAX=9000

# bindings
bindkey '^w' kill-region # FIXME need to detect mark-active for ^w to be nice
bindkey '\ed' kill-word
bindkey '\ef' emacs-forward-word
bindkey '\eb' emacs-backward-word
bindkey '\e[A' history-search-backward
bindkey '\e[B' history-search-forward

# special dirs
sp=~/src/semitenn/spacemmo
pl=~/src/blackfoundry/packetloop

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

# aliases
alias grin='grin --force-color'
alias less='less -R'
alias new='ls -lath $HOME/Downloads/ | head'

# resty
. $HOME/dotfiles/resty/resty

# I want ^S please
if test -n $PS1; then
  stty stop undef
fi

# just doit
#[[ -n "$(which brew)" ]] && source `brew --prefix`/etc/autojump
. ~/.zsh/cap_completion
#. ~/.zsh/title

[[ -f "$HOME/.rvm/scripts/rvm" ]] &&	\
	source "$HOME/.rvm/scripts/rvm"

[[ -f "$HOME/.amazon_keys" ]] &&	\
	source "$HOME/.amazon_keys"

[[ -f "$HOME/.ec2/pk.pem" ]] &&		\
	export EC2_PRIVATE_KEY="$HOME/.ec2/pk.pem"

[[ -f "$HOME/.ec2/cert.pem" ]] &&	\
	export EC2_CERT="$HOME/.ec2/cert.pem"

export EC2_HOME="/usr/local/Cellar/ec2-api-tools/1.4.4.1/jars"

export CLOBBER=1

# packetloop
export PL_KEYSPACE=PacketloopDevelopment

# https://gist.github.com/1688857
export RUBY_HEAP_MIN_SLOTS=1000000
export RUBY_HEAP_SLOTS_INCREMENT=1000000
export RUBY_HEAP_SLOTS_GROWTH_FACTOR=1
export RUBY_GC_MALLOC_LIMIT=1000000000
export RUBY_HEAP_FREE_MIN=500000

export NNTPSERVER=news.tpg.com.au

#. /usr/local/bin/virtualenvwrapper.sh

