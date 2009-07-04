" inspiration: http://github.com/foot/dotfiles/tree/master/.vimrc

" colours
colorscheme dave
syntax on

" options
set ts=2
set sts=2
set hidden
set shiftwidth=2
set nocompatible
set expandtab
set backspace=indent,eol,start
set guioptions-=m " menu
set guioptions+=c " console dialogues
set guioptions-=t " tearoff menu items
set guioptions-=T " toolbar
set guioptions-=L " left scroll (when vertically split)
set guioptions-=b " bottom scroll bar
set guioptions-=r " right scroll bar
set guioptions-=e " text mode tab lines
set mousemodel=popup
set viminfo='100,f1
set showtabline=2
set wildmode=list:longest,full
set shiftround
set autoindent
set smartindent
set completeopt=longest,menuone
set guifont=monaco\ 9

" use c-a in command mode
cnoremap <C-A> <Home>

" git/svn blame - \g/\s on a visual block
vmap <Leader>g :<C-U>!git blame <C-R>=expand("%") <CR> \| sed -n <C-R>=line("'<") <CR>,<C-R>=line("'>") <CR>p <CR>
vmap <Leader>s :<C-U>!svn blame <C-R>=expand("%") <CR> \| sed -n <C-R>=line("'<") <CR>,<C-R>=line("'>") <CR>p <CR>

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

" set paste! - keybindings
map <leader>p :set paste!<cr>

" fuzzy - dont use these modes
let g:fuzzy_ignore = ".git/*;.svn/*"
let g:FuzzyFinderOptions = {}
let g:FuzzyFinderOptions.Bookmark = {'mode_available': 0}
let g:FuzzyFinderOptions.Dir = {'mode_available': 0}
let g:FuzzyFinderOptions.MruFile = {'mode_available': 0}
let g:FuzzyFinderOptions.MruCmd = {'mode_available': 0}
let g:FuzzyFinderOptions.TaggedFile = {'mode_available': 0}
let g:FuzzyFinderOptions.Tag = { 'matching_limit': 20 }

" fuzzy - speed hax
let g:fuzzy_enumerating_limit = 20

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
  au FileType cs setlocal tabstop=4 shiftwidth=4 softtabstop=4 expandtab

  au BufNewFile,BufRead *.as setlocal filetype=actionscript
  au BufRead,BufNewFile *.json setlocal filetype=javascript
  au BufRead,BufNewFile Capfile setlocal filetype=ruby
augroup END

" autosave folds
aug views
  au!
  au BufWinLeave * nested silent! mkview
  au BufWinEnter * nested silent! loadview
aug END

" haskell
au BufEnter *.hs compiler ghc
let g:haddock_browser = "open"
let g:haddock_browser_callformat = "%s %s"

" lambda key and sum key
imap <c-a> λ
imap <c-s> ∑

" snippets
function! HighlightSnips()
     exec "hi snippetEmuJump guibg=grey30"
     exec "syn region snippetEmuJump start=/".g:snip_start_tag."/ end=/".g:snip_end_tag."/"
endfunction
augroup highlight-snips
    au BufNewFile,BufRead * call HighlightSnips()
augroup END

" camel case motion overrides
nmap <silent> <space> <Plug>CamelCaseMotion_w
omap <silent> <space> <Plug>CamelCaseMotion_w
vmap <silent> <space> <Plug>CamelCaseMotion_w

nmap <silent> <bs> <Plug>CamelCaseMotion_b
omap <silent> <bs> <Plug>CamelCaseMotion_b
vmap <silent> <bs> <Plug>CamelCaseMotion_b

omap <silent> i<space> <Plug>CamelCaseMotion_iw
vmap <silent> i<space> <Plug>CamelCaseMotion_iw
omap <silent> i<bs> <Plug>CamelCaseMotion_ib
vmap <silent> i<bs> <Plug>CamelCaseMotion_ib

" omni fail
inoremap <expr> <C-d> pumvisible() ? "\<PageDown>\<C-p>\<C-n>" : "\<C-d>"
inoremap <expr> <C-u> pumvisible() ? "\<PageUp>\<C-p>\<C-n>" : "\<C-u>"

