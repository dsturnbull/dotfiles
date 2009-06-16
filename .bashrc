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

export MXMLC_HOME=~/src/tilefile/flex
export PATH=$MXMLC_HOME/bin:$PATH

export JAVA_HOME=/System/Library/Frameworks/JavaVM.framework/Home

export IRONRUBY_HOME=~/src/ironruby
export PATH=$IRONRUBY_HOME/bin:$PATH

export PATH=$HOME/bin:$PATH
export PATH=$HOME/local/bin:$PATH
export PATH=$HOME/.cabal/bin:$PATH

export PATH=/opt/local/lib/postgresql83/bin:$PATH
export PATH=/opt/local/sbin:$PATH
export PATH=/opt/local/bin:$PATH
export PATH=/opt/ruby-enterprise/bin:$PATH

export PS1="\j \[\033[1;0m\][\[\033[00;34m\]\u\[\033[1;0m\]] \[\033[1;34m\]\h\[\033[00m\]: \[\033[00;36m\]\w\[\033[00m\] "
export EDITOR=vi
export MANPATH=$MANPATH:/opt/local/share/man
export NNTPSERVER=news.giganews.com
export CLICOLOR=1

if [ `uname` == "Linux" ]; then
  export PS1='\j \[\033[1;0m\][\[\033[00;34m\]\u\[\033[1;0m\]] \[\033[1;34m\]\h\[\033[00m\]: \[\033[00;36m\]\w\[\033[00m\]$(__git_ps1 " (%s)") $(git_status)\n→ '
elif [ `uname -m` == "iPhone1,2" ]; then
  export PS1='\j \[\033[1;0m\][\[\033[00;34m\]\u\[\033[1;0m\]] \[\033[1;34m\]\h\[\033[00m\]: \[\033[00;36m\]\w\[\033[00m\]\n→ '
else
  export PS1='\j \[\033[1;0m\][\[\033[00;34m\]\u\[\033[1;0m\]] \[\033[1;34m\]\h\[\033[00m\]: \[\033[00;36m\]\w\[\033[00m\]$(__git_ps1 " (%s)") $(git_status)\n→ '
fi

alias vi='vi'
alias grin='grin --force-color'
alias less='less -R'

alias chiron='mono /Applications/Silverlight/sdl-sdk/bin/Chiron.exe'
alias sl='/Applications/Silverlight/sdl-sdk/script/sl'
alias slserver='/Applications/Silverlight/sdl-sdk/script/server'

if test -n "$PS1"; then
  stty -ixon
fi

function git_status {
  if current_git_status=$(git status 2>/dev/null | grep 'added to commit'); then
    echo "☠"
  fi
}

. $HOME/dotfiles/resty/resty
. $HOME/.ec2/local_keys
