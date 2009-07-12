export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8
xmodmap $HOME/.Xmodmap
xset fp+ /usr/share/fonts/local
xset b off
Esetroot -scaled -center $HOME/dotfiles/High-Resolution-3-ALKVC8G2MB-1600x1200.jpg 
xbindkeys &
exec xmonad
