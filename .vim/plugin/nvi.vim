" File:		nvi.vim (global plugin)
" Description:	Simulate nvi's behaviour for u, . and <C-R>
" Author:	Antony Scriven	<ads@metawire.org>
" Version:	0.1
" Last Change:	2004-02-11
" URL:		<URL:http://www.vim.org/>
"
" Releases:
" 	0.1	Experimental.
"
" Installation:	Copy nvi.vim to your .vim/plugin directory (Unix) or
"		your vimfiles\plugin directory (Win) or similar.
"
" Usage:	This plugin messes with the behaviour of undo. So take
"		care!
"
"		`u' alternates between undo and redo. To repeat an undo
"		or redo press `.' like you would to repeat a change
"		command. <C-R> is mapped to it's rightful function of
"		redrawing the screen. If you want to repeat a change
"		command, `.' will still work as normal, even after using
"		it for undo. E.g. with the cursor on the first `a' below
"			abc abc x
"		dwdwryu..l. results in
"			ayc abc x
"
" Problems:	When pressing : or / or ? immediately after an undo or
"		redo, the command line isn't always redrawn immediately.
"		I'm not sure what is causing that. If you have 'showcmd'
"		set then the :, / or ? will appear as a partial command
"		in the statusline. Following input may be garbled
"		Following input may be garbled, but hitting <C-L> to
"		redraw fixes it. This looks like a vim problem.
"
"		Other cursor redrawing isn't perfect.
"
"		Because I fake the cursor with :match, sometimes a whole
"		tab character can be higlighted as the cursor position.
"
"		:redraw!s make it slow (authentic :-) over ssh/telnet.


" Prevent reloading.
if exists("g:loaded_nvi") | finish | endif | let g:loaded_nvi = 1

" Global settings.
se cpo+=u
nno <C-R> <C-L>

" Internal maps.
nno	<Plug>[NVIu]	u
nno	<Plug>[NVI:]	:
cno	<Plug>[NVIcr]	<CR>
"	<Plug>[NVInext] This is the map for the next character typed by
"			the user.

" Fake the cursor when in the command line.
nno     <Plug>[cur]	:match cursor /\%#/<CR>
nno     <Plug>[nocur]	:match NONE<CR>


" Define some functions for ugly maps to make the code easier to read
" later on.

fun! s:Mapu()

	nmap <silent> u <Plug>[NVIu]<Plug>[cur]<Plug>[NVI:]redraw!<Plug>[NVIcr]<Plug>[NVI:]call <SID>Handle_u()<Plug>[NVIcr]<Plug>[NVInext]

endfun


fun! s:Mapnext()

	nmap <Plug>[NVInext] <Plug>[NVI:]call <SID>Handle_dot()<Plug>[NVIcr]<Plug>[NVInext]

endfun


" Initialize.
call s:Mapu()


fun! s:Handle_u()
	" After the user presses `u', process the next character typed.

	" Clear the fake cursor in case <C-C> is pressed (you can't map
	" it). But it won't actually be visible until <C-C> is pressed
	" and the screen is updated.
	match NONE

	" Unlike in s:Handle_dot, I need to explicitly bring things to a
	" halt if there is nothing further to undo or redo.
	if !&modified
		nmap <Plug>[NVInext] <Nop>
		redraw!
		return
	endif

	call inputsave()
	let c = nr2char(getchar())
	call inputrestore()

	if c == '.'
		exe "norm! \<C-R>"
		match cursor /\%#/
		redraw!
		call s:Mapnext()
	else
		match NONE
		redraw!
		exe "nmap <Plug>[NVInext] " . c
	endif

endfun


fun! s:Handle_dot()
	" After the user presses `.', process the next character typed.

	" Clear the fake cursor in case <C-C>.
	match NONE

	call inputsave()
	let c = nr2char(getchar())
	call inputrestore()

	" Note that there is no loop to handle multiple presses of `.'
	" The map itself contains the loop until the else block
	" redefines it. It seems natural to just let <C-R> fail when
	" there is nothing left to redo or undo. The map will just
	" abort.
	if c == '.'
		exe "norm! \<C-R>"
		match cursor /\%#/
		redraw!
	else
		exe 'nmap <Plug>[NVInext] ' . c
		call s:Mapu()
		match NONE
		redraw!
	endif

endfun


" vim: se ts=8 sw=8 noet sts=0 tw=72 :
