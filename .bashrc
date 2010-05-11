if [ -f /bin/zsh ]; then
  zsh && exit
fi

# xquartz dickery
cd

if [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi

# 290ms
if [ -f /opt/local/etc/bash_completion ]; then
    . /opt/local/etc/bash_completion
fi

# less tabs
bind 'set completion-query-items 3000' # don't ask, show them all
bind 'set show-all-if-ambiguous on'    # don't require two tabs
bind 'set page-completions off'        # no pager for long completion lists, show them all at once
bind 'set visible-stats on'            # append file type characters to completion items

# fruity colours for less
export LESS_TERMCAP_mb=$'\E[01;31m'
export LESS_TERMCAP_md=$'\E[01;31m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_so=$'\E[01;44;33m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01;32m'

# history less shit
shopt -s histappend
PROMPT_COMMAND='history -a'

# /ect/ -> /etc/
shopt -s dirspell 2>/dev/null

# expand history completion by pressing space
bind space:magic-space

# .../really/long/5/is/enough
export PROMPT_DIRTRIM=5

# Ignore these on completion
export FIGNORE=CVS:.svn:.DS_Store:.git:.gitignore

export EC2_PRIVATE_KEY=~/.ec2/pk.pem
export EC2_CERT=~/.ec2/cert.pem

export EC2_HOME=$HOME/src/ec2-api-tools-1.3-46266
export AMI_HOME=$HOME/src/ec2-ami-tools-1.3-31780
export ELB_HOME=$HOME/src/ElasticLoadBalancing-1.0
export AUTO_SCALING_HOME=$HOME/src/AutoScaling-1.0
export MAGLEV_HOME=$HOME/src/MagLev-installer/MagLev-22384.Solaris-x86
export PATH=$MAGLEV_HOME/bin:$EC2_HOME/bin:$AMI_HOME/bin:$ELB_HOME/bin:$AUTO_SCALING_HOME/bin:$PATH


export MXMLC_HOME=$HOME/src/flex_sdk_3
export PATH=$MXMLC_HOME/bin:$PATH

# LINUX FLAGS
if [ `uname` == "Linux" ]; then
  export JAVA_HOME=/usr/lib/jvm/java-6-sun-1.6.0.13
else
  export JAVA_HOME=/System/Library/Frameworks/JavaVM.framework/Home
fi

# DARWIN FLAGS
if [ `uname` == "Darwin" ]; then
  export ARCHFLAGS="-arch x86_64"
fi

export ANDROID_HOME=~/src/android-sdk-mac_86
export PATH=$ANDROID_HOME/tools:$PATH

export IRONRUBY_HOME=~/src/ironruby
export PATH=$IRONRUBY_HOME/bin:$PATH

export PATH=$HOME/src/depot_tools:$PATH

export PATH=/opt/local/lib/postgresql83/bin:$PATH
export PATH=/opt/local/lib/postgresql84/bin:$PATH
export PATH=/opt/local/sbin:$PATH
export PATH=/opt/local/bin:$PATH
export PATH=/opt/ruby-enterprise/bin:$PATH

export PATH=$HOME/bin:$PATH
export PATH=$HOME/local/bin:$PATH
export PATH=$HOME/.cabal/bin:$PATH

# hardcoded for speed
GEM_PATH=/Users/dave/.gem/ruby/1.8/bin:/opt/local/lib/ruby/gems/1.8/bin
export PATH=$GEM_PATH:$PATH

# osol stuff
export EDITOR=vi
export PAGER=less
export MANPATH=/opt/SUNWut/share/man:/opt/csw/share/man:$MANPATH:/opt/local/share/man:/usr/gnu/share/man:/usr/share/man:/usr/X11/share/man
export PATH=/opt/csw/bin:/opt/SUNWut/sbin:/opt/SUNWut/bin:/opt/DTT/Bin:/usr/local/bin:/usr/gnu/bin:/usr/bin:/usr/X11/bin:/usr/sbin:/sbin:/usr/sfw/bin/:$HOME/bin:$PATH

# too confusing now.
export PATH=/opt/local/bin:/opt/local/sbin:$PATH

export NNTPSERVER=news.giganews.com
export CLICOLOR=1
if [ $TERM == "rxvt-unicode" ]; then
  export TERM=xterm-256color
fi
export LANG=en_US.UTF-8
export USERWM=`which xmonad 2>/dev/null`

alias grin='grin --force-color'
alias less='less -R'
alias new='ls -lath $HOME/Downloads/ | head'

alias chiron='mono /Applications/Silverlight/sdl-sdk/bin/Chiron.exe'
alias sl='/Applications/Silverlight/sdl-sdk/script/sl'
alias slserver='/Applications/Silverlight/sdl-sdk/script/server'

. $HOME/dotfiles/resty/resty

if [ -e $HOME/.ec2/local_keys ]; then
  . $HOME/.ec2/local_keys
fi

## git status prompt
# Get the name of the branch we are on
function git_prompt_info {
  determine_git_ps1_availability

  branch_prompt=$(__git_ps1 "$@")
  if [ -n "$branch_prompt" ]; then
    current_git_status=$(git status)
    if dirty=$(echo "$current_git_status" | grep 'added to commit' 2> /dev/null); then
      branch_prompt="$branch_prompt*"
    fi
    if behind_by=$(echo "$current_git_status" | grep 'behind .* [0-9]\+ commit'); then
      behind_by=$(echo "$behind_by" | awk '{print $8}')
      branch_prompt="$branch_prompt -$behind_by"
    fi
    if ahead_by=$(echo "$current_git_status" | grep 'ahead .* [0-9]\+ commit'); then
      ahead_by=$(echo "$current_git_status" | grep 'ahead .* [0-9]\+ commit' | awk '{print $9}')
      branch_prompt="$branch_prompt +$ahead_by"
    fi
    echo -e "$branch_prompt"
  fi
}


## vi -t [tab][tab]
_vim_ctags() {
    local cur prev

    COMPREPLY=()
    cur="${COMP_WORDS[COMP_CWORD]}"
    prev="${COMP_WORDS[COMP_CWORD-1]}"

    case "${prev}" in
        -t)
            # Avoid the complaint message when no tags file exists
            if [ ! -r ./tags ]
            then
                return
            fi

            # Escape slashes to avoid confusing awk
            cur=${cur////\\/}

            COMPREPLY=( $(compgen -W "`awk -v ORS=" "  "/^${cur}/ { print \\$1 }" tags`" ) )
            ;;
        *)
            # Perform usual completion mode
            ;;
    esac
}

# Files matching this pattern are excluded
excludelist='*.@(o|O|so|SO|so.!(conf)|SO.!(CONF)|a|A|rpm|RPM|deb|DEB|gif|GIF|jp?(e)g|JP?(E)G|mp3|MP3|mp?(e)g|MP?(E)G|avi|AVI|asf|ASF|ogg|OGG|class|CLASS)'

complete -F _vim_ctags -f -X "${excludelist}" vi vim gvim rvim view rview rgvim rgview gview
_vim_ctags() {
    local cur prev

    COMPREPLY=()
    cur="${COMP_WORDS[COMP_CWORD]}"
    prev="${COMP_WORDS[COMP_CWORD-1]}"

    case "${prev}" in
        -t)
            # Avoid the complaint message when no tags file exists
            if [ ! -r ./tags ]
            then
                return
            fi

            # Escape slashes to avoid confusing awk
            cur=${cur////\\/}

            COMPREPLY=( $(compgen -W "`awk -v ORS=" "  "/^${cur}/ { print \\$1 }" tags`" ) )
            ;;
        *)
            # Perform usual completion mode
            ;;
    esac
}

# Files matching this pattern are excluded
excludelist='*.@(o|O|so|SO|so.!(conf)|SO.!(CONF)|a|A|rpm|RPM|deb|DEB|gif|GIF|jp?(e)g|JP?(E)G|mp3|MP3|mp?(e)g|MP?(E)G|avi|AVI|asf|ASF|ogg|OGG|class|CLASS)'

complete -F _vim_ctags -f -X "${excludelist}" vi vim gvim rvim view rview rgvim rgview gview

function determine_git_ps1_availability {
  type __git_ps1 2>/dev/null | grep 'is a function' > /dev/null
  if [ $? -ne 0 ]; then
    function __git_ps1 {
      return 0
    }
  fi
}

function do_ps1 {
# PS1 prompt
  NICE_ORANGE="\[\033[38;5;208m\]"
  NICE_BLUE="\[\033[38;5;39m\]"
  NICE_GREEN="\[\033[38;5;154m\]"
  GRAY="\[\033[1;30m\]"
  LIGHT_GRAY="\[\033[0;37m\]"
  CYAN="\[\033[0;36m\]"
  LIGHT_CYAN="\[\033[1;36m\]"
  NO_COLOUR="\[\033[0m\]"
  GREEN="\[\033[01;32m\]"
  BLUE="\[\033[01;34m\]"
  LIGHT_BLUE="\[\033[01;36m\]"

  #PS1="\j $NO_COLOUR[$NICE_ORANGE\u$NO_COLOUR] $NICE_GREEN\h$NO_COLOUR:$NICE_BLUE\w$NO_COLOUR\$(git_prompt_info)\n→ "
  if [ `uname` == "SunOS" ]; then
    EXTRA_LINE_CHAR=">"
  else
    EXTRA_LINE_CHAR="→"
  fi

  PS1="\j \[\033[1;0m\][\[\033[00;33m\]\u\[\033[1;0m\]] \[\033[1;34m\]\h\[\033[00m\]: \[\033[00;36m\]\w\[\033[00m\]\$(git_prompt_info)\n$EXTRA_LINE_CHAR "
}

if test -n "$PS1"; then
  stty -ixon
  # give back <c-s> to forward search (opposite of c-r)
  stty stop undef
fi

do_ps1


test -r /sw/bin/init.sh && . /sw/bin/init.sh

