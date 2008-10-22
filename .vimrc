" http://github.com/foot/dotfiles/tree/master/.vimrc
"
colorscheme tango

syntax on
set ts=2
set sts=2
set shiftwidth=2
set nocompatible
set expandtab
set backspace=indent,eol,start
set guioptions-=m
set guioptions-=L
set guioptions-=r
set guioptions-=T
set guioptions+=c
set guifont=Bitstream\ Vera\ Sans\ Mono\ 9

cnoremap <C-A> <Home>

vmap <Leader>a :<C-U>!svn blame <C-R>=expand("%") <CR> \| sed -n <C-R>=line("'<") <CR>,<C-R>=line("'>") <CR>p <CR>

cmap w!! %!sudo tee > /dev/null %
inoremap # X#

noremap <C-k> <C-W>k
noremap <C-H> <C-W>h
noremap <C-L> <C-W>l
noremap <C-j> <C-W>j

nmap X ci"

imap <M-j> <Esc>:m+<CR>gi
imap <M-k> <Esc>:m-2<CR>gi
vmap <M-k> :m'<-2<CR>gv
vmap <M-h> :<<CR>gv
nmap <M-j> mz:m+<CR>`z
nmap <M-k> mz:m-2<CR>`z
vmap <M-j> :m'>+<CR>gv
vmap <M-l> :><CR>gv

set wildmode=list:longest,full

filetype on
filetype indent on
filetype plugin on

augroup init
  au FileType python setlocal textwidth=79 tabstop=4 shiftwidth=4 softtabstop=4 expandtab
  au FileType ruby setlocal textwidth=79 tabstop=2 shiftwidth=2 softtabstop=2 expandtab
  au FileType javascript setlocal tabstop=4 shiftwidth=4 softtabstop=4 expandtab

  au BufNewFile,BufRead *.as setlocal filetype=actionscript
  au BufRead,BufNewFile *.json setlocal filetype=javascript
  au BufRead,BufNewFile Capfile setlocal filetype=ruby
augroup END

set shiftround
set autoindent
set smartindent

nmap <M-c> :!ctags -R .

cnoremap <M-BS> <C-W>

nmap <c-e> :FuzzyFinderTag<cr>
nmap <c-s> :FuzzyFinderBuffer<cr>
nmap <c-f> :FuzzyFinderFile \*\*\/<cr>
 
" Dont use these modes.
let g:FuzzyFinderOptions = {}
let g:FuzzyFinderOptions.Dir = {'mode_available': 0}
let g:FuzzyFinderOptions.MruFile = {'mode_available': 0}
let g:FuzzyFinderOptions.MruCmd = {'mode_available': 0}
let g:FuzzyFinderOptions.FavFile = {'mode_available': 0}
let g:FuzzyFinderOptions.TaggedFile = {'mode_available': 0}
 
" Change open key so we're 'pulling down' new file into current window.
let g:FuzzyFinderOptions.Base = {}
let g:FuzzyFinderOptions.Base.key_open = '<c-j>'
let g:FuzzyFinderOptions.Base.key_open_split = '<CR>'
 
" key_next_mode is already <c-l>, change key_prev_mode to matching <c-h>
let g:FuzzyFinderOptions.Base.key_prev_mode = '<C-h>'

" speed hax
let g:fuzzy_matching_limit = 20

" textmate
map <leader>t :FuzzyFinderTextMate<CR>

let g:vimirc_nick = "dave_vim"
let g:vimirc_user = "dave_vim"
let g:vimirc_realname = "David Turnbull"
let g:vimirc_server = "irc.meobets.com:6667"
