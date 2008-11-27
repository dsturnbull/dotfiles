if [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi

if [ -f /opt/local/etc/bash_completion ]; then
    . /opt/local/etc/bash_completion
fi

export EC2_HOME=$HOME/src/ec2-api-tools-1.3-19403
export EC2_PRIVATE_KEY=~/.ec2/pk-YQZTGHR2ENHCTZCHZF2ZBQRSLQZTQAPM.pem
export EC2_CERT=~/.ec2/cert-YQZTGHR2ENHCTZCHZF2ZBQRSLQZTQAPM.pem
export MXMLC_HOME=~/src/flex

export PATH=$HOME/bin:$PATH
export PATH=$HOME/local/bin:$PATH
export PATH=$HOME/.gem/ruby/1.8/bin:$PATH
export PATH=$HOME/.cabal/bin:$PATH
export PATH=$MXMLC_HOME/bin:$PATH
export PATH=$EC2_HOME/bin:$PATH
export PATH=/opt/ruby-enterprise-1.8.6-20080810/bin:$PATH
export PATH=/opt/local/bin:$PATH

if [ `uname` == "Linux" ]; then
  export JAVA_HOME=/usr/lib/jvm/java-6-sun-1.6.0.06/jre
fi

alias vi='vi'
alias grin='grin --force-color'
alias less='less -R'
export EDITOR=vi

if test -n "$PS1"; then
  stty -ixon
fi

function git_status {
  if current_git_status=$(git status 2>/dev/null | grep 'added to commit'); then
    echo "☠"
  fi
}

export PS1='\j \[\033[1;0m\][\[\033[00;34m\]\u\[\033[1;0m\]] \[\033[1;34m\]\h\[\033[00m\]: \[\033[00;36m\]\w\[\033[00m\]$(__git_ps1 " (%s)") $(git_status)\n→ '
