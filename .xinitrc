export LANG=en_US.UTF-8
source $HOME/.bashrc
xmodmap $HOME/.xmodmap
quartz-wm --proxy-only &
xsetroot -solid black
xset b off
Esetroot -scaled -center $HOME/dotfiles/High-Resolution-3-ALKVC8G2MB-1600x1200.jpg 
exec /Users/dave/.cabal/bin/xmonad
