#!/usr/bin/env python
# coding: iso-8859-1

from vim  import *
from math import *
from re   import *

# CONFIGURATION ___________________________________________________________________________________
aa        = " .,:+*&%#08@"  # ASCII-art character set
rmargin   = 1               # Distance from gfx to right edge of window
tmargin   = 16              # Distance from gfx to top edge of buffer
bmargin   = 0               # Distance from gfx to bottom edge of buffer
w         = current.window.width
b         = current.buffer
lenlist   = range(len(b))   # FIXME: This looks stupid. How do I declare a list without this hack?
smoothing = 24              # Smoothing on left edge of gfx

def clear():     # Remove the results of beautify()
    for l in range(len(b)):
        pattern = compile(" *#[" + aa + "]*$") # all lines that end with commented ASCII-art chars
        b[l] = sub(pattern, "", b[l])          # remove them

def spaces(l):   # Return an indentation value interpolated from current indentation values
    sum = 0
    for n in range(-smoothing/2,smoothing/2+1):       # sum the lengths of nearby lines
        if   l+n < 0       : sum += lenlist[0]        # above the top of the buffer
        elif l+n >= len(b) : sum += lenlist[len(b)-1] # below the bottom of the buffer
        else               : sum += lenlist[l+n]
    return max(1, int(sum / (smoothing+1) + w/4 - len(b[l])) )

def beautify(f): # add comments with ASCII art pattern at the end of every line
    clear()
    for l in range(len(b)): lenlist[l] = len(b[l]) # save all old line lengths
    for l in range(tmargin,len(b)-bmargin):
        # do we have space to draw gfx on this line?
        if w-rmargin > len(b[l]): b[l] += " " * spaces(l) + "# "
        for x in range(len(b[l]),w-rmargin):   # from left edge of text to right margin
            col = f(x,l)                       # apply function
            if   (col < 0)   : col = 0         # cut extreme colour values
            elif (col > .99) : col = 0.99
            b[l] += aa[ int( col * len(aa) ) ] # ASCII artify colour

# EXAMPLE FUNCTIONS _______________________________________________________________________________
shade     = lambda col,x,y: col - float(w-x) / w
righthalf = lambda colf,x,y: (x>(w/2)) * col

wave    = lambda col,x,y: col + sin( y * .2 ) / 10 + .5
int2    = lambda col,x,y: col + (x * y) % 40 / 40. - .2
stripe  = lambda col,x,y: col + (x + y) % 10 / 10. - .2
circles = lambda col,x,y: col + (((x-2*w/3)**2 + (y*1.8-43)**2)**.5) % 8 < 4
int1    = lambda col,x,y: col + ( sin( x / 8.) + 2 + sin( y * x / 2.)) ** ( sin( y / 7. ) + 1 ) / 16.
blob    = lambda col,x,y: col + ( sin( x * .5 + sin(  x * .23 + y * .21 ))
                      + sin( y * .5 + sin( -x * .23 + y * .21 ))) / 2. + .2

# MAIN PROGRAM ____________________________________________________________________________________
combination = lambda x,y : shade(circles(0,x,y)/4.+blob(0,x,y)*0.5+0.3,x,y)

beautify(combination)
#clear() # uncomment this to remove all beautification

