# all shells

# ffu
export JAVA_HOME=/Library/Java/Home

# path
PATH=/usr/local/bin:/usr/local/sbin:$PATH

# x11
export PATH=/usr/X11/bin:$PATH

# frameworks, platforms
export ANDROID_SDK_ROOT=/usr/local/Cellar/android-sdk/r20.0.3

# user bin paths
export PATH=$HOME/bin:$PATH
export PATH=$HOME/local/bin:$PATH
export PATH=$HOME/.cabal/bin:$PATH
export PATH=$PATH:$HOME/src/jruby/bin
export PATH=$PATH:/Users/david/src/play/depot_tools

export HADOOP_OPTS="-Djava.security.krb5.realm=OX.AC.UK -Djava.security.krb5.kdc=kdc0.ox.ac.uk:kdc1.ox.ac.uk"

# mrtoolkit
#export PATH=/Library/Ruby/Gems/1.8/gems/mrtoolkit-0.1.2/standalone:$PATH

# node
export NODE_PATH=/usr/local/lib/node

# what
export PAGER=less

PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting

#export http_proxy=http://localhost:3128
#export HTTP_PROXY=http://localhost:3128
#export https_proxy=https://localhost:3128
#export HTTPS_PROXY=https://localhost:3128

export LANG=en_AU.UTF-8
export LC_ALL=en_AU.UTF-8
#printf '\e]701;%s\007' $LANG
