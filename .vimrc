" vim:filetype=vim
" inspiration: http://github.com/foot/dotfiles/tree/master/.vimrc

" colours
set t_Co=256
syntax on
colorscheme leo


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


" textwidth colouring
"set textwidth=79
"match ErrorMsg '\%>80v.\+'
"au BufWinEnter * let w:m1=matchadd('ErrorMsg', '\%>80v.\+', -1)


" chut up tabs
au BufWinEnter * let g:TabLineSet_verbose = 'buffers_list'


" vundles
set rtp+=~/.vim/vundle.git/
call vundle#rc()


" github repos

" Vim/Ruby Configuration Files
Bundle 'vim-ruby/vim-ruby'

" Copy syntax-highlighted code from vim to the OS X clipboard as RTF text
Bundle 'aniero/vim-copy-as-rtf.git'

" Ruby on Rails power tools
Bundle 'tpope/vim-rails'

" It's like rails.vim without the rails
Bundle 'tpope/vim-rake'

" a Git wrapper so awesome, it should be illegal
Bundle 'tpope/vim-fugitive'
set laststatus=2
set statusline=%<%f\ %h%m%r%=%-14.(%l,%c%V%)\ %P\ %{fugitive#statusline()}

" Vim Cucumber runtime files
Bundle 'tpope/vim-cucumber'

" easily search for, substitute, and abbreviate multiple variants of a word
Bundle 'tpope/vim-abolish'

" fuzzyfinder plugin to support TextMate style file searches
Bundle 'jamis/fuzzyfinder_textmate'

" use clang for completing C/C++ code
Bundle 'Rip-Rip/clang_complete'

" visualize your Vim undo tree.
Bundle 'sjl/gundo.vim'

" xpath -> html
Bundle 'rstacruz/sparkup', {'rtp': 'vim/'}

" vim-scripts repos

" Motion through CamelCaseWords and underscore_notation
Bundle 'camelcasemotion'

" dependency of FuzzyFinder
Bundle 'L9'

" buffer/file/command/tag/etc explorer with fuzzy matching
Bundle 'FuzzyFinder'

" Automatically opens popup menu for completions
Bundle 'AutoComplPop'

" A Vim7 tabline customization
Bundle 'TabLineSet.vim'

" Plugin for the Perl module / CLI script 'ack'
Bundle 'ack.vim'

" Alternate Files quickly (.c --> .h etc)
Bundle 'a.vim'

" Automated Tag creation/inclusion
" TODO: crashes lots
" Bundle 'Intelligent-Tags'

" Source code browser (supports C/C++, java, perl, python, tcl, sql, php, etc)
Bundle 'taglist.vim'

" Simulate a split shell, using gnu screen or tmux, that you can send commands to.
" use tmux, not screen
let g:ScreenImpl = 'Tmux'
Bundle 'Screen-vim---gnu-screentmux'

" A plugin that helps with switching between single-line and multiline code
Bundle 'splitjoin.vim'

" Plugin to supplement id-utils gid command for super-fast find-in-files.
Bundle 'IDSearch'

" Finally, the power of Vim + Figlet!
Bundle 'Figlet.vim'

" Update automatically the serial of DNS Zone
Bundle 'UpdateDNSSerial'

" Plugin for C/C++/ObjC/ObjC++ include directive completion.
let g:inccomplete_findcmd = 'gfind'
"Bundle 'inccomplete'

" Twitter client for Vim
"Bundle 'TwitVim'

" Draw ascii stuff
Bundle 'DrawIt'

" Comment stuff easily
Bundle 'tComment'

" Surround text
Bundle 'surround.vim'

" Super fast motion
Bundle 'EasyMotion'
let g:EasyMotion_leader_key = ','

" non github repos

" CommandT
Bundle 'git://git.wincent.com/command-t.git'

" rfc5424 (syslog)
" Bundle 'https://code.google.com/p/vim-syntax-rfc5424/'


" use c-a in command mode
cnoremap <C-a> <Home>


" split/join
nmap <leader>j :SplitjoinJoin<CR>
nmap <leader>o :SplitjoinSplit<CR>


" git/svn blame - \g/\s on a visual block - FIXME svn blame not working
vmap <leader>g :<C-u>!git blame <C-r>=expand("%") <CR> \| sed -n <C-r>=line("'<") <CR>,<C-r>=line("'>") <CR>p <CR>
vmap <leader>s :<C-u>!svn blame <C-r>=expand("%") <CR> \| sed -n <C-r>=line("'<") <CR>,<C-r>=line("'>") <CR>p <CR>


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


" don't save options in view
set viewoptions-=options


" autosave folds
au BufWinLeave * nested silent! mkview
au BufWinEnter * nested silent! loadview


" alternates
nmap <D-[> :A<CR>
nmap <D-]> :R<CR>


" testing
nmap <leader>q :.Rake<CR>
nmap <leader>w :Rake<CR>


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
nmap <leader>\ :ptag <C-r>=expand("<cword>")<CR><CR>


" remove highlighting
" nmap <leader>\ :nohlsearch<CR>


" -fblocks
hi link cErrInParen Normal


" what
set modelines=1


" Append modeline after last line in buffer.
" Use substitute() instead of printf() to handle '%%s' modeline in LaTeX
" files.
function! AppendModeline()
  let l:modeline = "// vim: ts=4:sw=4:sts=4:et"
  call append(line("$"), l:modeline)
endfunction
nnoremap <silent> <leader>ml :call AppendModeline()<CR>


" clang
let g:clang_complete_copen = 1
let g:clang_periodic_quickfix = 1
let g:clang_library_path = '/Developer/usr/clang-ide/lib'
let g:clang_use_library = 1
let g:clang_complete_macros = 1
let g:clang_complete_patterns = 1
"let g:clang_debug = 1
set updatetime=200
nmap <leader>c :call g:ClangUpdateQuickFix()<CR>
nmap <leader>x :ccl<CR>


" gundo
nmap <leader>b :GundoToggle<CR>


" taglist
nmap <leader>m :TlistToggle<CR>


" include path
set path=,,.,/usr/include,/usr/local/include


" idutils gid
nmap <leader>g :call g:IDSearchCurrentWord()<CR>


