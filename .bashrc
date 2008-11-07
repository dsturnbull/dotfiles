if [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi

export EC2_HOME=$HOME/src/ec2-api-tools-1.3-19403
export EC2_PRIVATE_KEY=~/.ec2/pk-YQZTGHR2ENHCTZCHZF2ZBQRSLQZTQAPM.pem
export EC2_CERT=~/.ec2/cert-YQZTGHR2ENHCTZCHZF2ZBQRSLQZTQAPM.pem
export MXMLC_HOME=~/src/flex
export CLASSPATH=$HOME/src/classpath
export PATH=$MXMLC_HOME/bin:$EC2_HOME/bin:$PATH:$HOME/bin:$HOME/local/bin:$HOME/.gem/ruby/1.8/bin

export PATH=/opt/ruby-enterprise/bin:/opt/local/bin:$HOME/local/bin:$PATH:$HOME/bin:$HOME/.cabal/bin:$HOME/src/git-issues

if [ `uname` == "Linux" ]; then
  export JAVA_HOME=/usr/lib/jvm/java-6-sun-1.6.0.06/jre
fi

alias vi='vi'
export EDITOR=vi

echo $PS1
if test -n "$PS1"; then
  stty -ixon
fi

export PS1="\j \[\033[1;0m\][\[\033[00;34m\]\u\[\033[1;0m\]] \[\033[1;34m\]\h\[\033[00m\]: \[\033[00;36m\]\w\[\033[00m\] "
