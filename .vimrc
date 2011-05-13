" vim:filetype=vim
" inspiration: http://github.com/foot/dotfiles/tree/master/.vimrc

" colours
syntax on

" options
set autoindent                    " current line indent carries to next line
set smartindent                   " also pay attention to syntax
set shiftround                    " round indent to multiple of sw
set hidden                        " do not unload buffers which go out of visibility
set nocompatible                  " yeah
set backspace=indent,eol,start    " backspace multi lines
set viminfo='100,f1               " marks remembered for 100 files, enable mark storing
set wrap                          " for julio
set showtabline=2                 " always
set wildmode=list:longest,full    " completion style when opening files

" autoindent
set formatoptions+=r              " keep autoindent for <CR>
set formatoptions-=o              " but stop it when o/O
set formatoptions+=t              " autowrap text to textwidth

" yes
set textwidth=79
match ErrorMsg '\%>80v.\+'
au BufWinEnter * let w:m1=matchadd('ErrorMsg', '\%>80v.\+', -1)

" chut up tabs
au BufWinEnter * let g:TabLineSet_verbose = 'modified'

" vundles
set rtp+=~/.vim/vundle.git/ 
call vundle#rc()

" github repos
Bundle 'vim-ruby/vim-ruby'
Bundle 'tpope/vim-rails'
Bundle 'tpope/vim-fugitive'
Bundle 'jamis/fuzzyfinder_textmate'

" vim-scripts repos
Bundle 'camelcasemotion'
Bundle 'L9'
Bundle 'FuzzyFinder'
Bundle 'AutoComplPop'
Bundle 'TabLineSet.vim'
Bundle 'ack.vim'

" non github repos
Bundle 'git://git.wincent.com/command-t.git'

" use c-a in command mode
cnoremap <C-a> <Home>

" git/svn blame - \g/\s on a visual block - FIXME svn blame not working
vmap <Leader>g :<C-u>!git blame <C-r>=expand("%") <CR> \| sed -n <C-r>=line("'<") <CR>,<C-r>=line("'>") <CR>p <CR>
vmap <Leader>s :<C-u>!svn blame <C-r>=expand("%") <CR> \| sed -n <C-r>=line("'<") <CR>,<C-r>=line("'>") <CR>p <CR>

" move windows easily
noremap <C-k> <C-w>k
noremap <C-h> <C-w>h
noremap <C-l> <C-w>l
noremap <C-j> <C-w>j

" slide text around
" imap <Up>    <Esc>:m-2<CR>gi
" imap <Down>  <Esc>:m+<CR>gi
" 
" nmap <Up>    mz:m-2<CR>`z
" nmap <Down>  mz:m+<CR>`z
" 
" vmap <Up>    :m'<-2<CR>gv
" vmap <Down>  :m'>+<CR>gv
" vmap <Left>  :<<CR>gv
" vmap <Right> :><CR>gv

" redo ctags and fuzzy cache
nmap <leader>T :call Fctags_rescan()<CR>
function! Fctags_rescan()
   exec "!ctags -R ."
   exec "CommandTFlush"
endfunction

" leader key bindings
map <leader>e :FufTag<CR>
map <leader>s :CommandTBuffer<CR>
map <leader>t :CommandT<CR>
map <leader>p :set paste!<CR>
map <leader>r :Ack

" indentation
filetype plugin indent on

aug init
  au FileType ruby      let g:rubycomplete_rails=1
  au FileType ruby      let g:rubycomplete_classes_in_global=1
aug END

" don't save options in view
set viewoptions-=options

" autosave folds
au BufWinLeave * nested silent! mkview
au BufWinEnter * nested silent! loadview

nmap <Up> zk
nmap <Down> zj
nmap <Right> zo
nmap <Left> zc

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

" align commas nicely
" FIXME it's bad
function! AlignCommasNicely()
  exec ":AlignCtrl =Wl"
  exec ":%Align ,"
  exec ":%s/\\([ ]\\+\\),/,\\1/g"
endfunction
command! AC :call AlignCommasNicely()

" tab hax
nmap <silent> <C-n> :tabn<CR>
nmap <silent> <C-p> :tabp<CR>
" swap tag stack pop with tabnew
nmap <C-BSlash> :po<CR>
nmap <silent> <C-t> :tabnew<CR>
nmap <SwipeUp> :tabp<CR>
nmap <SwipeDown> :tabp<CR>

" counterparts
nmap <SwipeUp> :split<CR>
nmap <SwipeDown> :split<CR><C-w>j<CR>
nmap <SwipeLeft> <C-w>v<CR>
nmap <SwipeRight> <C-w>v<CR><C-w>l<CR>

" ptag
nmap <Leader>\ :ptag <C-r>=expand("<cword>")<CR><CR>

" -fblocks
hi link cErrInParen Normal 

set modelines=1

" Append modeline after last line in buffer.
" Use substitute() instead of printf() to handle '%%s' modeline in LaTeX
" files.
function! AppendModeline()
  let l:modeline = "// vim: ts=4:sw=4:sts=4:et"
  call append(line("$"), l:modeline)
endfunction
nnoremap <silent> <Leader>ml :call AppendModeline()<CR>
