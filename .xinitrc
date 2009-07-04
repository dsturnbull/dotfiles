export LANG=en_US.UTF-8
source $HOME/.bashrc
xmodmap $HOME/.xmodmap
quartz-wm --proxy-only &
xsetroot -solid black
xset b off
exec /Users/dave/.cabal/bin/xmonad
