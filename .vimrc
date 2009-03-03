" inspiration: http://github.com/foot/dotfiles/tree/master/.vimrc

" colours
colorscheme dave
syntax on

" options
set background=dark
set ts=2
set sts=2
set hidden
set shiftwidth=2
set nocompatible
set expandtab
set backspace=indent,eol,start
set guioptions-=m
set guioptions-=L
set guioptions-=r
set guioptions-=T
set guioptions-=a
set guioptions-=A
set guioptions=c
set mousemodel=popup
set guifont=Bitstream\ Vera\ Sans\ Mono\ 7
set viminfo='100,f1
set showtabline=2
set wildmode=list:longest,full
set shiftround
set autoindent
set smartindent

" use c-a in command mode
cnoremap <C-A> <Home>

" git blame - \a on a visual block
vmap <Leader>a :<C-U>!git blame <C-R>=expand("%") <CR> \| sed -n <C-R>=line("'<") <CR>,<C-R>=line("'>") <CR>p <CR>

" sudo writes
cmap w!! %!sudo tee > /dev/null %

" ?
inoremap # X#
nmap X ci"
cnoremap <M-BS> <C-W>

" move windows easily
noremap <C-k> <C-W>k
noremap <C-H> <C-W>h
noremap <C-L> <C-W>l
noremap <C-j> <C-W>j

" taglist bindings
nmap <c-o> :TlistOpen<CR>
nmap <c-i> :TlistToggle<CR>

" slide text around
imap <M-j> <Esc>:m+<CR>gi
imap <M-k> <Esc>:m-2<CR>gi
vmap <M-k> :m'<-2<CR>gv
vmap <M-h> :<<CR>gv
nmap <M-j> mz:m+<CR>`z
nmap <M-k> mz:m-2<CR>`z
vmap <M-j> :m'>+<CR>gv
vmap <M-l> :><CR>gv

" redo ctags
nmap <M-c> :!ctags -R .

" fuzzy - keybindings
nmap <c-e> :FuzzyFinderTag<cr>
nmap <c-s> :FuzzyFinderBuffer<cr>
nmap <c-f> :FuzzyFinderFile \*\*\/<cr>
map <leader>t :FuzzyFinderTextMate<CR>

" fuzzy - dont use these modes
let g:FuzzyFinderOptions = {}
let g:FuzzyFinderOptions.Dir = {'mode_available': 0}
let g:FuzzyFinderOptions.MruFile = {'mode_available': 0}
let g:FuzzyFinderOptions.MruCmd = {'mode_available': 0}
let g:FuzzyFinderOptions.FavFile = {'mode_available': 0}
let g:FuzzyFinderOptions.TaggedFile = {'mode_available': 0}
 
" fuzzy - speed hax
let g:fuzzy_matching_limit = 20

" Basically you press * or # to search for the current selection !! Really useful
vnoremap <silent> * :call VisualSearch('f')<CR>
vnoremap <silent> # :call VisualSearch('b')<CR>
 
" turn on hlsearch when searching for something explicitly
nnoremap * :set hlsearch<cr>*
nnoremap # :set hlsearch<cr>#
"nnoremap / :set hlsearch<cr>/
"nnoremap ? :set hlsearch<cr>?
" turn hlsearch OFF
nmap <Leader><Leader> :set hlsearch!<cr>
nmap <Leader>/ :set hlsearch!<cr>
" TODO: turn OFF when using search as a motion
" onoremap / :set nohlsearch<cr>/

" tab hax
nmap <c-n> :tabn<CR>
nmap <c-p> :tabp<CR>

" indentation
filetype on
filetype indent on
filetype plugin on

augroup init
  au FileType python setlocal tabstop=4 shiftwidth=4 softtabstop=4 expandtab
  au FileType ruby setlocal tabstop=2 shiftwidth=2 softtabstop=2 expandtab
  au FileType javascript setlocal tabstop=4 shiftwidth=4 softtabstop=4 expandtab
  au FileType haskell setlocal tabstop=4 shiftwidth=4 softtabstop=4 expandtab
  au FileType cpp setlocal tabstop=4 shiftwidth=4 softtabstop=4 expandtab
  au FileType c setlocal tabstop=4 shiftwidth=4 softtabstop=4 expandtab

  au BufNewFile,BufRead *.as setlocal filetype=actionscript
  au BufRead,BufNewFile *.json setlocal filetype=javascript
  au BufRead,BufNewFile Capfile setlocal filetype=ruby
augroup END

" visual search
" From an idea by Michael Naumann (via xpaulbettsx)
" RT @foot
function! VisualSearch(direction) range
  let l:saved_reg = @"
  execute "normal! vgvy"
  let l:pattern = escape(@", '\\/.*$^~[]')
  let l:pattern = substitute(l:pattern, "\n$", "", "")
  if a:direction == 'b'
    execute "normal ?" . l:pattern . "^M"
  else
    execute "normal /" . l:pattern . "^M"
  endif
  let @/ = l:pattern
  let @" = l:saved_reg
endfunction
 
" vim -b : edit binary using xxd-format!
augroup Binary
  au!
  au BufReadPre  *.pyc let &bin=1
  au BufReadPost *.pyc if &bin | %!xxd
  au BufReadPost *.pyc set ft=xxd | endif
  au BufWritePre *.pyc if &bin | %!xxd -r
  au BufWritePre *.pyc endif
  au BufWritePost *.pyc if &bin | %!xxd
  au BufWritePost *.pyc set nomod | endif
augroup END

" autosave folds
aug views
  au!
  au BufWinLeave * nested mkview
  au BufWinEnter * nested silent! loadview
aug END

" taglist
let Tlist_Inc_Winwidth = 0
let Tlist_Sort_Type = "order"
"let Tlist_Use_Horiz_Window = 1
"let Tlist_WinHeight = 20
let Tlist_WinWidth = 40
let Tlist_Display_Tag_Scope = 0

