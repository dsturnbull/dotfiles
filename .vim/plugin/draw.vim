" Vim plugin for drawing rectangles and lines using blockwise Visual selection.
" Maintainer:	Timo Frenay <timo@frenay.net>
" Last Change:	2002 Oct 20

" This plugin allows you to draw rectangles and lines using blockwise Visual
" mode, which makes it ideal for drawing tables and such. Simply move the cursor
" to the start position, hit CTRL-V to enter blockwise Visual mode and move the
" cursor to define the rectangle or line. Finally, type <leader>d to draw the
" rectangle or line. If you don't know what <leader> is, \d should do the trick.
" This script supports UTF-8 as well as PC-8 boxdrawing characters, and checks
" the value of 'encoding' to determine which it should use. If you use a single
" byte character set other than ECS/PC-8 which includes boxdrawing characters,
" please notify me so I can add support for it.
"
" Note: I strongly recommend ":set virtualedit+=block" when using this script,
" so you can define rectangles beyond the end of the line.

" Map <leader>d in Visual mode to the DrawRect() function below.
vnoremap <silent> <leader>d  <Esc>:call <SID>DrawRect()<CR>

" Bitwise OR table for combining UTF-8 boxdrawing characters.
let s:UTF8_table = "--02001402020C1C0018003410242C3C0202141402021C1C1818343424"
    \. "243C3C001400140C1C0C1C003400342C3C2C3C141414141C1C1C1C343434343C3C3C3C"
    \. "02020C1C02020C1C10242C3C10242C3C02021C1C02021C1C24243C3C24243C3C0C1C0C"
    \. "1C0C1C0C1C2C3C2C3C2C3C2C3C1C1C1C1C1C1C1C1C3C3C3C3C3C3C3C3C001800341024"
    \. "2C3C0018003410242C3C1818343424243C3C1818343424243C3C003400342C3C2C3C00"
    \. "3400342C3C2C3C343434343C3C3C3C343434343C3C3C3C10242C3C10242C3C10242C3C"
    \. "10242C3C24243C3C24243C3C24243C3C24243C3C2C3C2C3C2C3C2C3C2C3C2C3C2C3C2C"
    \. "3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C"

" Bitwise OR table for combining PC-8 boxdrawing characters.
let s:PC8_table = "---179196192179179218195196217196193191180194197179179192192"
    \. "179179195195217217193193180180197197196192196192218195218195196193196"
    \. "193194197194197192192192192195195195195193193193193197197197197179179"
    \. "218195179179218195191180194197191180194197179179195195179179195195180"
    \. "180197197180180197197218195218195218195218195194197194197194197194197"
    \. "195195195195195195195195197197197197197197197197196217196193191180194"
    \. "197196217196193191180194197217217193193180180197197217217193193180180"
    \. "197197196193196193194197194197196193196193194197194197193193193193197"
    \. "197197197193193193193197197197197191180194197191180194197191180194197"
    \. "191180194197180180197197180180197197180180197197180180197197194197194"
    \. "197194197194197194197194197194197194197197197197197197197197197197197"
    \. "197197197197197197"

function! <SID>DrawRect()
  if (visualmode() != "\<C-V>")
    " Beep!
    execute "normal! \<Esc>"
    echoerr "DrawRect() requires a blockwise Visual selection"
    return
  endif
  " Backup options.
  let l:virtualedit = &virtualedit
  let l:wrap = &wrap
  " Set options.
  set virtualedit=all nowrap
  " Get rectangle boundaries.
  let l:top = line("'<")
  let l:left = virtcol("'<")
  let l:bottom = line("'>")
  let l:right = virtcol("'>")
  if (l:top == l:bottom)
    normal! `<
    if (l:left == l:right)
      " Draw a cross. I hope that's what you wanted...
      call <SID>DrawCode(15)
    else 
      " Draw a horizontal line.
      call <SID>DrawCode(2)
      let l:count = l:right - l:left - 1
      while (l:count)
        normal! l
        call <SID>DrawCode(10)
        let l:count = l:count - 1
      endwhile
      normal! l
      call <SID>DrawCode(8)
    endif
  elseif (l:left == l:right)
    " Draw a vertical line.
    normal! `<
    call <SID>DrawCode(4)
    let l:count = l:bottom - l:top - 1
    while (l:count)
      normal! j
      call <SID>DrawCode(5)
      let l:count = l:count - 1
    endwhile
    normal! j
    call <SID>DrawCode(1)
  else
    " Draw a rectangle.
    if (l:right < l:left)
      " Blockwise Visual selection is right-to-left, mirror it first.
      execute "normal! gvO\<Esc>"
      let l:left = virtcol("'<")
      let l:right = virtcol("'>")
    endif
    " Draw bottom-right corner.
    normal! `>
    call <SID>DrawCode(9)
    " Draw bottom side.
    let l:count = l:right - l:left - 1
    while (l:count)
      normal! h
      call <SID>DrawCode(10)
      let l:count = l:count - 1
    endwhile
    " Draw bottom-left corner.
    normal! h
    call <SID>DrawCode(3)
    " Draw left side.
    let l:count = l:bottom - l:top - 1
    while (l:count)
      normal! k
      call <SID>DrawCode(5)
      let l:count = l:count - 1
    endwhile
    " Draw top-left corner.
    normal! k
    call <SID>DrawCode(6)
    " Draw top side.
    let l:count = l:right - l:left - 1
    while (l:count)
      normal! l
      call <SID>DrawCode(10)
      let l:count = l:count - 1
    endwhile
    " Draw top-right corner.
    normal! l
    call <SID>DrawCode(12)
    " Draw right side.
    let l:count = l:bottom - l:top - 1
    while (l:count)
      normal! j
      call <SID>DrawCode(5)
      let l:count = l:count - 1
    endwhile
    " Return the cursor to the end of the Visual selection.
    normal! j
  endif
  " Restore options.
  let &virtualedit = l:virtualedit
  let &wrap = l:wrap
endfunction

" Draw the boxdrawing character with the specified drawing code at the cursor
" position, combining it with any boxdrawing character under the cursor.
function! <SID>DrawCode(code)
  if (a:code < 1) || (a:code > 15)
    " Beep!
    execute "normal! \<Esc>"
    echoerr "Invalid drawing code:" a:code
    return
  endif
  " Backup "c register.
  let l:c = @c
  " Get the character under the cursor.
  normal! "cyl
  let l:char = @c
  " Get the index of the combined character in one of the bitwise OR tables.
  let l:index = <SID>Char2Code(l:char) + 16 * a:code
  " Look up the character in the respective table.
  if (&encoding =~ 'utf\|ucs')
    " UTF-8 encoding.
    let l:char = "u25" . strpart(s:UTF8_table, l:index * 2, 2)
  else
    " PC-8 encoding.
    let l:char = strpart(s:PC8_table, l:index * 3, 3)
  endif
  " Replace the character under the cursor.
  execute "normal! r\<C-V>" . l:char
  " Restore "c register.
  let @c = l:c
endfunction

" Get the drawing code of the specified character. If it is not a recognized
" boxdrawing character in the current encoding, 0 is returned.
function! <SID>Char2Code(char)
  let l:code = char2nr(a:char)
  if (&encoding =~ 'utf\|ucs')
    " UTF-8 encoding.
    if (l:code == 9492)
      return 3
    elseif (l:code == 9474)
      return 5
    elseif (l:code == 9484)
      return 6
    elseif (l:code == 9500)
      return 7
    elseif (l:code == 9496)
      return 9
    elseif (l:code == 9472)
      return 10
    elseif (l:code == 9524)
      return 11
    elseif (l:code == 9488)
      return 12
    elseif (l:code == 9508)
      return 13
    elseif (l:code == 9516)
      return 14
    elseif (l:code == 9532)
      return 15
    else
      return 0
    endif
  else
    " PC-8 encoding.
    if (l:code == 192)
      return 3
    elseif (l:code == 179)
      return 5
    elseif (l:code == 218)
      return 6
    elseif (l:code == 195)
      return 7
    elseif (l:code == 217)
      return 9
    elseif (l:code == 196)
      return 10
    elseif (l:code == 193)
      return 11
    elseif (l:code == 191)
      return 12
    elseif (l:code == 180)
      return 13
    elseif (l:code == 194)
      return 14
    elseif (l:code == 197)
      return 15
    else
      return 0
    endif
  endif
endfunction

" vim: ts=8
