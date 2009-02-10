if [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi

if [ -f /opt/local/etc/bash_completion ]; then
    . /opt/local/etc/bash_completion
fi

export EC2_HOME=$HOME/src/ec2-api-tools-1.3-19403
export EC2_PRIVATE_KEY=~/.ec2/pk-YQZTGHR2ENHCTZCHZF2ZBQRSLQZTQAPM.pem
export EC2_CERT=~/.ec2/cert-YQZTGHR2ENHCTZCHZF2ZBQRSLQZTQAPM.pem
export MXMLC_HOME=~/src/tilefile/flex
export PATH=$MXMLC_HOME/bin:$EC2_HOME/bin:$PATH:$HOME/bin
export PS1="\j \[\033[1;0m\][\[\033[00;34m\]\u\[\033[1;0m\]] \[\033[1;34m\]\h\[\033[00m\]: \[\033[00;36m\]\w\[\033[00m\] "
export EDITOR=vi
export CRYSTAL=/Library/CrystalSpace
export CEL=/Library/CEL
export PATH=$CRYSTAL/bin:/opt/local/bin:/opt/ruby-enterprise/bin:$HOME/local/bin:$PATH:$HOME/bin:$HOME/.cabal/bin

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
export EDITOR=vi

if test -n "$PS1"; then
  stty -ixon
fi

function git_status {
  if current_git_status=$(git status 2>/dev/null | grep 'added to commit'); then
    echo "☠"
  fi
}
