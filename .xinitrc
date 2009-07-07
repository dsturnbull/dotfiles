export LANG=en_US.UTF-8
source $HOME/.bashrc
xmodmap $HOME/.Xmodmap
quartz-wm --proxy-only &
xsetroot -solid black
xset b off
Esetroot -scaled -center $HOME/dotfiles/High-Resolution-3-ALKVC8G2MB-1600x1200.jpg 
xhost +localhost
#echo "ssh darkstar.redcordial.org" | xclip
exec $HOME/.cabal/bin/xmonad
