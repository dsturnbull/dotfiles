# all shells

# ffu
export JAVA_HOME=/Library/Java/Home

# path
PATH=/usr/local/bin:/usr/local/sbin:$PATH

# x11
export PATH=/usr/X11/bin:$PATH

# frameworks, platforms
export ANDROID_HOME=~/src/android-sdk-mac_86
export PATH=$ANDROID_HOME/tools:$PATH

# user bin paths
export PATH=$HOME/bin:$PATH
export PATH=$HOME/local/bin:$PATH
export PATH=$HOME/.cabal/bin:$PATH
export PATH=$PATH:$HOME/src/jruby/bin

export HADOOP_OPTS="-Djava.security.krb5.realm=OX.AC.UK -Djava.security.krb5.kdc=kdc0.ox.ac.uk:kdc1.ox.ac.uk"

# mrtoolkit
#export PATH=/Library/Ruby/Gems/1.8/gems/mrtoolkit-0.1.2/standalone:$PATH

# node
export NODE_PATH=/usr/local/lib/node

# what
export PAGER=less

PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting
