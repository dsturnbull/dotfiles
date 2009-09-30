" vim:filetype=vim
" inspiration: http://github.com/foot/dotfiles/tree/master/.vimrc
" check for updates:
"   snipMate snippets: git://github.com/scrooloose/snipmate-snippets.git
"   snipMate: http://www.vim.org/scripts/script.php?script_id=2540
"   rails.vim: http://www.vim.org/scripts/script.php?script_id=1567

" colours
syntax on
colorscheme leo

" options
set tabstop=2                     " tab stop
set softtabstop=2                 " soft tab stop
set shiftwidth=2                  " indent/outdent
set expandtab                     " spaces, not tabs
set shiftround                    " round indent to multiple of sw
set autoindent                    " current line indent carries to next line
set smartindent                   " also pay attention to syntax
set hidden                        " do not unload buffers which go out of visibility
set nocompatible                  " yeah
set backspace=indent,eol,start    " backspace multi lines
set guioptions-=m                 " no menu
set guioptions+=c                 " console dialogues (no gui popups)
set guioptions-=t                 " tearoff menu items
set guioptions-=T                 " no toolbar
set guioptions-=L                 " no left scroll (when vertically split)
set guioptions-=b                 " no bottom scroll bar
set guioptions-=r                 " no right scroll bar
set guioptions-=e                 " text mode tab lines
set mousemodel=extend             " right click extends selection
set mouse=a                       " mouse selection in normal, command and insert modes
set clipboard=autoselect,unnamed
set viminfo='100,f1,%             " marks remembered for 100 files, enable mark storing, buffers stored
                                  " FIXME % doesn't work
set showtabline=2                 " always
set wildmode=list:longest,full    " completion style
set completeopt=longest,menuone   " sort by longest, show when single match
set guifont=terminus\ 9           " yay fonts
set transparency=30               " yay web 2.0

" FIXME dnw comment autoindent
inoremap # X#

" use c-a in command mode
cnoremap <C-a> <Home>

" w!!
cmap w!! %!sudo tee > /dev/null %

" git/svn blame - \g/\s on a visual block
vmap <Leader>g :<C-u>!git blame <C-r>=expand("%") <CR> \| sed -n <C-r>=line("'<") <CR>,<C-r>=line("'>") <CR>p <CR>
vmap <Leader>s :<C-u>!svn blame <C-r>=expand("%") <CR> \| sed -n <C-r>=line("'<") <CR>,<C-r>=line("'>") <CR>p <CR>

" sudo writes
cmap w!! %!sudo tee > /dev/null %

" FIXME ffffuuuu no M on osx?
cnoremap <A-BS> <C-w>

" move windows easily
noremap <C-k> <C-w>k
noremap <C-h> <C-w>h
noremap <C-l> <C-w>l
noremap <C-j> <C-w>j

" slide text around
imap <Up>    <Esc>:m-2<CR>gi
imap <Down>  <Esc>:m+<CR>gi

nmap <Up>    mz:m-2<CR>`z
nmap <Down>  mz:m+<CR>`z

vmap <Up>    :m'<-2<CR>gv
vmap <Down>  :m'>+<CR>gv
vmap <Left>  :<<CR>gv
vmap <Right> :><CR>gv

" redo ctags and fuzzy cache
nmap <leader>T :call Fctags_and_fuzzy_rescan()<CR>
function! Fctags_and_fuzzy_rescan()
   exec "!ctags -R ."
   exec "ruby finder.rescan!"
endfunction

" fuzzy - keybindings
map <leader>e :FuzzyFinderTag<CR>
map <leader>s :FuzzyFinderBuffer<CR>
map <leader>f :FuzzyFinderFile \*\*\/<CR>
map <leader>t :FuzzyFinderTextMate<CR>

" set paste! - keybindings
map <leader>p :set paste!<CR>

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
nmap <C-n> :tabn<CR>
nmap <C-p> :tabp<CR>
" swap tag stack pop with tavnew
nmap <C-BSlash> :po<CR>
nmap <C-t> :tabnew<CR>

" indentation
filetype on
filetype indent on
filetype plugin on

aug init
  au FileType python     setlocal tabstop=4 shiftwidth=4 softtabstop=4 expandtab
  au FileType ruby       setlocal tabstop=2 shiftwidth=2 softtabstop=2 expandtab
  au FileType javascript setlocal tabstop=4 shiftwidth=4 softtabstop=4 expandtab
  au FileType haskell    setlocal tabstop=4 shiftwidth=4 softtabstop=4 expandtab
  au FileType cpp        setlocal tabstop=4 shiftwidth=4 softtabstop=4 expandtab
  au FileType c          setlocal tabstop=4 shiftwidth=4 softtabstop=4 expandtab
  au FileType cs         setlocal tabstop=4 shiftwidth=4 softtabstop=4 expandtab
  au FileType sh         setlocal tabstop=4 shiftwidth=4 softtabstop=4 expandtab
  au FileType objc       setlocal tabstop=4 shiftwidth=4 softtabstop=4 expandtab

  au FileType ruby       let g:rubycomplete_rails=1
  au FileType ruby       let g:rubycomplete_classes_in_global=1

  au BufNewFile,BufRead *.as    setlocal filetype=actionscript
  au BufRead,BufNewFile *.json  setlocal filetype=javascript
  au BufRead,BufNewFile Capfile setlocal filetype=ruby
aug END

" autosave folds
au BufWritePost * nested silent! mkview
au BufReadPost  * nested silent! loadview

" haskell
au BufEnter *.hs compiler ghc
let g:haddock_browser = "open"
let g:haddock_browser_callformat = "%s %s"

" lambda key and sum key
imap <C-a> λ
imap <C-s> ∑

" camel case motion overrides
nmap <silent> <Space> <Plug>CamelCaseMotion_w
omap <silent> <Space> <Plug>CamelCaseMotion_w
vmap <silent> <Space> <Plug>CamelCaseMotion_w

nmap <silent> <BS> <Plug>CamelCaseMotion_b
omap <silent> <BS> <Plug>CamelCaseMotion_b
vmap <silent> <BS> <Plug>CamelCaseMotion_b

omap <silent> i<Space> <Plug>CamelCaseMotion_iw
vmap <silent> i<Space> <Plug>CamelCaseMotion_iw
omap <silent> i<BS>    <Plug>CamelCaseMotion_ib
vmap <silent> i<BS>    <Plug>CamelCaseMotion_ib

" omni fail
imap <C-]> <C-x><C-]>
inoremap <expr> <C-d> pumvisible() ? "\<PageDown>\<C-p>\<C-n>" : "\<C-d>"
inoremap <expr> <C-u> pumvisible() ? "\<PageUp>\<C-p>\<C-n>" : "\<C-u>"

" fold text between all contexts and specify lines
function! ShowRSpecAnnotation()
 call cursor('$', 0)
 try
   foldo!
 catch
 endtry
 let cur_line = line('$')
 while cur_line > 0
   let prev_spec = search('it\s\+["''].\+["'']', 'Wb', '^')
   if ! prev_spec
     break
   endif
   exec (prev_spec).','.cur_line.'fold'
   let cur_line=prev_spec-1
 endwhile
endfunction
command! Sa :call ShowRSpecAnnotation()
