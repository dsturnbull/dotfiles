# crazy XQuartz
cd

if [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi

if [ -f /opt/local/etc/bash_completion ]; then
    . /opt/local/etc/bash_completion
fi

export EC2_PRIVATE_KEY=~/.ec2/pk.pem
export EC2_CERT=~/.ec2/cert.pem

export EC2_HOME=$HOME/src/ec2-api-tools-1.3-36506
export AMI_HOME=$HOME/src/ec2-ami-tools-1.3-31780
export ELB_HOME=$HOME/src/ElasticLoadBalancing-1.0
export AUTO_SCALING_HOME=$HOME/src/AutoScaling-1.0
export PATH=$EC2_HOME/bin:$AMI_HOME/bin:$ELB_HOME/bin:$AUTO_SCALING_HOME/bin:$PATH

export MXMLC_HOME=$HOME/src/flex_sdk_3
export PATH=$MXMLC_HOME/bin:$PATH

if [ `uname` == "Linux" ]; then
  export JAVA_HOME=/usr/lib/jvm/java-6-sun-1.6.0.13
else
  export JAVA_HOME=/System/Library/Frameworks/JavaVM.framework/Home
fi

export IRONRUBY_HOME=~/src/ironruby
export PATH=$IRONRUBY_HOME/bin:$PATH

export PATH=/opt/local/lib/postgresql83/bin:$PATH
export PATH=/opt/local/sbin:$PATH
export PATH=/opt/local/bin:$PATH
export PATH=/opt/ruby-enterprise/bin:$PATH

export PATH=$HOME/bin:$PATH
export PATH=$HOME/local/bin:$PATH
export PATH=$HOME/.cabal/bin:$PATH

GEM_PATH=$(gem env path | sed -e 's/:/\/bin:/g' -e 's/$/\/bin/')
export PATH=$GEM_PATH:$PATH

export PS1="\j \[\033[1;0m\][\[\033[00;34m\]\u\[\033[1;0m\]] \[\033[1;34m\]\h\[\033[00m\]: \[\033[00;36m\]\w\[\033[00m\] "
export EDITOR=vi
export MANPATH=$MANPATH:/opt/local/share/man
export NNTPSERVER=news.giganews.com
export CLICOLOR=1

if [ `uname` == "Linux" ]; then
  export PS1='\j \[\033[1;0m\][\[\033[00;34m\]\u\[\033[1;0m\]] \[\033[1;34m\]\h\[\033[00m\]: \[\033[00;36m\]\w\[\033[00m\]$(git_prompt_info)\n→ '
elif [ `uname -m` == "iPhone1,2" ]; then
  export PS1='\j \[\033[1;0m\][\[\033[00;34m\]\u\[\033[1;0m\]] \[\033[1;34m\]\h\[\033[00m\]: \[\033[00;36m\]\w\[\033[00m\]\n→ '
else
  export PS1='\j \[\033[1;0m\][\[\033[00;34m\]\u\[\033[1;0m\]] \[\033[1;34m\]\h\[\033[00m\]: \[\033[00;36m\]\w\[\033[00m\]$(git_prompt_info)\n→ '
fi

if [ $TERM == "rxvt-unicode" ]; then
  export TERM=xterm-color
fi
export LANG=en_US.UTF-8
export USERWM=`which xmonad`

alias vi='vi'
alias grin='grin --force-color'
alias less='less -R'

alias chiron='mono /Applications/Silverlight/sdl-sdk/bin/Chiron.exe'
alias sl='/Applications/Silverlight/sdl-sdk/script/sl'
alias slserver='/Applications/Silverlight/sdl-sdk/script/server'

if test -n "$PS1"; then
  stty -ixon
  # give back <c-s> to forward search (opposite of c-r)
  stty stop undef
fi

. $HOME/dotfiles/resty/resty
. $HOME/.ec2/local_keys

## git status prompt
# Get the name of the branch we are on
function git_prompt_info {
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

