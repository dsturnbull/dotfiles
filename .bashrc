if [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi

export PS1='\@ \u@\h/\j [\w] '

export EC2_HOME=$HOME/src/ec2-api-tools-1.3-19403
export JAVA_HOME=/usr/lib/jvm/java-6-sun-1.6.0.06/jre
export EC2_PRIVATE_KEY=~/.ec2/pk-YQZTGHR2ENHCTZCHZF2ZBQRSLQZTQAPM.pem
export EC2_CERT=~/.ec2/cert-YQZTGHR2ENHCTZCHZF2ZBQRSLQZTQAPM.pem
export MXMLC_HOME=~/src/tilefile/flex
export PATH=$MXMLC_HOME/bin:$EC2_HOME/bin:$PATH:$HOME/bin
export PS1="\j \[\033[1;0m\][\[\033[00;34m\]\u\[\033[1;0m\]] \[\033[1;34m\]\h\[\033[00m\]: \[\033[00;36m\]\w\[\033[00m\] "

export PATH=/opt/ruby-enterprise/bin:$HOME/local/bin:$PATH:$HOME/bin:$HOME/.cabal/bin:/opt/local/bin
alias vi='mvim'
