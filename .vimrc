" vim:filetype=vim
" inspiration: http://github.com/foot/dotfiles/tree/master/.vimrc

" colours
syntax on
colorscheme BusyBee

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
set clipboard=autoselect,unnamed

" autoindent
set formatoptions+=r              " keep autoindent for <CR>
set formatoptions-=o              " but stop it when o/O
set formatoptions+=t              " autowrap text to textwidth

" CHUT UP
set vb

" yes
"set textwidth=79
match ErrorMsg '\%>80v.\+'
au BufWinEnter * let w:m1=matchadd('ErrorMsg', '\%>80v.\+', -1)

" chut up tabs
au BufWinEnter * let g:TabLineSet_verbose = 'buffers_list'

" vundles
set rtp+=~/.vim/vundle.git/ 
call vundle#rc()

" github repos
Bundle 'vim-ruby/vim-ruby'
Bundle 'tpope/vim-rails'
Bundle 'tpope/vim-rake'
Bundle 'tpope/vim-fugitive'
Bundle 'tpope/vim-cucumber'
Bundle 'jamis/fuzzyfinder_textmate'
Bundle 'Rip-Rip/clang_complete'

" vim-scripts repos
Bundle 'camelcasemotion'
Bundle 'L9'
Bundle 'FuzzyFinder'
Bundle 'AutoComplPop'
Bundle 'TabLineSet.vim'
Bundle 'ack.vim'
Bundle 'abolish.vim'
Bundle 'a.vim'

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

" alternates
nmap <D-[> :A<CR>
nmap <D-]> :R<CR>

" remove highlighting
nmap <Leader>\ :nohlsearch<CR>

" testing
nmap <Leader>q :.Rake<CR>
nmap <Leader>w :Rake<CR>

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
" nmap <Leader>\ :ptag <C-r>=expand("<cword>")<CR><CR>

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

let g:clang_auto_select = 1
let g:clang_complete_copen = 1
let g:clang_periodic_quickfix = 1
let g:clang_library_path = '/Developer/usr/clang-ide/lib'
let g:clang_use_library = 1
set updatetime=200

nmap <Leader>c :call g:ClangUpdateQuickFix()<CR>
nmap <Leader>x :ccl<CR>

" why you not work?
augroup ClangComplete
  au! BufWritePost *.c call s<SID>DoPeriodicQuickFix()
augroup end

" Protect large files from sourcing and other overhead.
" Files become read only
if !exists("my_auto_commands_loaded")
  let my_auto_commands_loaded = 1
  " Large files are > 10M
  " Set options:
  " eventignore+=FileType (no syntax highlighting etc
  " assumes FileType always on)
  " noswapfile (save copy of file)
  " bufhidden=unload (save memory when other file is viewed)
  " buftype=nowritefile (is read-only)
  " undolevels=-1 (no undo possible)
  let g:LargeFile = 1024 * 1024 * 10
  augroup LargeFile
    autocmd BufReadPre * let f=expand("<afile>") | if getfsize(f) > g:LargeFile | set eventignore+=FileType | setlocal noswapfile bufhidden=unload buftype=nowrite undolevels=-1 | else | set eventignore-=FileType | endif
  augroup END
end
