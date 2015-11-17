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
"set relativenumber
set undofile
set undodir=$HOME/.vim/.VIM_UNDO_FILES
set undolevels=50000
set virtualedit=block
set noshowmode
set laststatus=2
set statusline=%<%f\ %h%m%r%=%-14.(%l,%c%V%)\ %P\ %{fugitive#statusline()}

" yay mouse
set mouse=a

" autoindent
set formatoptions+=r              " keep autoindent for <CR>
set formatoptions-=o              " but stop it when o/O
set formatoptions+=t              " autowrap text to textwidth

" CHUT UP
set vb

set shell=zsh

" textwidth colouring
"set textwidth=79
"set colorcolumn=80
"match ErrorMsg '\%>80v.\+'
"au BufWinEnter * let w:m1=matchadd('ErrorMsg', '\%>80v.\+', -1)

" chut up tabs
"au BufWinEnter * let g:TabLineSet_verbose = 'buffers_list'

" vundles
set rtp+=~/.vim/vundle.git/
call vundle#rc()

" vim-scripts repos
  " gist plugin
  "Bundle 'Gist.vim'

  " Motion through CamelCaseWords and underscore_notation
  Bundle 'camelcasemotion'

  " buffer explorer
  " Bundle 'bufexplorer.zip'

  " Automatically opens popup menu for completions
  Bundle 'AutoComplPop'

  " A Vim7 tabline customization
  "Bundle 'TabLineSet.vim'

  " Plugin for the Perl module / CLI script 'ack'
  Bundle 'ack.vim'
  " nmap <Leader>a :Tabularize /
  " nmap <Leader>a= :Tabularize / = <CR>
  " nmap <Leader>a: :Tabularize /:\zs<CR>
  nmap <Leader>rn :cn<CR>
  nmap <Leader>rp :cp<CR>
  nmap <Leader>ro :cw<CR>
  nmap <Leader>rf :cnf<CR>
  nmap <Leader>rqo :cw<CR>
  nmap <Leader>rqq :ccl<CR>
  nmap <Leader>rlo :lw<CR>
  nmap <Leader>rlq :lcl<CR>
  nmap <Leader>gs :Gstatus<CR>
  nmap <Leader>gb :Gblame<CR>

  " Alternate Files quickly (.c --> .h etc)
  Bundle 'a.vim'

  " Automated Tag creation/inclusion
  " TODO: crashes lots
  " Bundle 'Intelligent-Tags'

  " Source code browser (supports C/C++, java, perl, python, tcl, sql, php, etc)
  "Bundle 'taglist.vim'

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

  " Draw ascii stuff
  Bundle 'DrawIt'

  " Comment stuff easily
  Bundle 'tComment'

  " Surround text
  Bundle 'surround.vim'

  " Super fast motion
  Bundle 'EasyMotion'
  let g:EasyMotion_leader_key = ','

  " Pig Latin syntax
  Bundle 'pig.vim'

" github repos
  " Vim/Ruby Configuration Files
  Bundle 'vim-ruby/vim-ruby'

  " multiple cursors
  " Bundle 'terryma/vim-multiple-cursors'

  " JIZZ
  " Bundle 'Lokaltog/vim-powerline.git'
  " let g:Powerline_symbols = 'fancy'
  Bundle 'bling/vim-airline'
  " let g:airline_powerline_fonts = 1

  " show list of buffers in the command bar
  Bundle 'bling/vim-bufferline'
  let g:bufferline_echo = 0

  " Copy syntax-highlighted code from vim to the OS X clipboard as RTF text
  Bundle 'aniero/vim-copy-as-rtf.git'

  " Ruby on Rails power tools
  Bundle 'tpope/vim-rails'

  " It's like rails.vim without the rails
  Bundle 'tpope/vim-rake'

  " syntax checking
  " Bundle 'scrooloose/syntastic'

  " repl
  Bundle 'jpalardy/vim-slime'

  " ocaml
  " Bundle 'the-lambda-church/merlin'

  " idris
  Bundle 'idris-hackers/idris-vim'

  Bundle 'Shougo/vimproc'
  Bundle 'Shougo/vimshell'

  " a Git wrapper so awesome, it should be illegal
  Bundle 'tpope/vim-fugitive'

  " edit icons in vim
  Bundle 'tpope/vim-afterimage'

  " Vim Cucumber runtime files
  Bundle 'tpope/vim-cucumber'

  " easily search for, substitute, and abbreviate multiple variants of a word
  Bundle 'tpope/vim-abolish'

  " use clang for completing C/C++ code
  " Bundle 'Rip-Rip/clang_complete'

  " visualize your Vim undo tree.
  Bundle 'sjl/gundo.vim'

  " detect end/endif etc
  "Bundle 'tpope/vim-endwise'

  " ag
  Bundle 'rking/ag.vim'

  " more fuzzy shit
  Bundle 'kien/ctrlp.vim'
  let g:ctrlp_prompt_mappings = {
    \ 'PrtSelectMove("j")':   ['<c-j>', '<c-n>'],
    \ 'PrtSelectMove("k")':   ['<c-k>', '<c-p>'],
    \ 'PrtHistory(-1)':       ['<down>'],
    \ 'PrtHistory(1)':        ['<up>'],
    \ }

  " let g:ctrlp_match_window_bottom = 0
  let g:ctrlp_match_window_reversed = 0
  let g:ctrlp_max_height = 40
  let g:ctrlp_dotfiles = 0
  "let g:ctrlp_user_command = ['.git/', 'cd %s && git ls-files']
  let g:ctrlp_extensions = ['tag', 'buffertag', 'quickfix', 'dir']
  let g:ctrlp_map = '-'
  " let g:ctrlp_lazy_update = 1
  let g:ctrlp_custom_ignore = {
    \ 'dir':  '\.git$\|\.hg$\|\.svn\|target$',
    \ 'file': '\.exe$\|\.so$\|\.dll$\|\.o$\|\.d$\|\.pyc$\|\.dylib$\|\.sys$',
    \ 'link': 'SOME_BAD_SYMBOLIC_LINKS',
    \ }
  let g:ctrlp_working_path_mode = 'r'

  map <leader>b :CtrlPBuffer<CR>
  map <leader>e :CtrlPTag<CR>
  map <leader>f :CtrlP<CR>
  map <leader>t :CtrlPMixed<CR>

  " % extensions
  Bundle 'edsono/vim-matchit'

  " tabulate data
  Bundle 'godlygeek/tabular'

  " i(ndent) text object
  Bundle 'michaeljsmith/vim-indent-object'

  " PEP8 MTEHRUFKCER
  Bundle 'pep8'

  " json
  Bundle 'elzr/vim-json'

  " automatically update tags
  " Bundle 'easytags.vim'
  " let g:easytags_updatetime_autodisable = 1
  " "let g:easytags_updatetime_min = 200
  " let g:easytags_python_enabled = 1
  " set tags=./tags;
  " let g:easytags_dynamic_files = 1

  " tiling
  " Bundle 'spolu/dwm.vim'

  " scala syntax
  Bundle 'derekwyatt/vim-scala'

" non github repos
  " CommandT
  " Bundle 'git://git.wincent.com/command-t.git'
  " map <leader>t :CommandT<CR>
  " map <leader>f :CommandTFlush<CR>

  " rfc5424 (syslog)
  " Bundle 'https://code.google.com/p/vim-syntax-rfc5424/'

  " notes
  " Bundle 'fmoralesc/vim-pad'
  " let g:pad#silent_on_mappings_fail = 1
  " let g:pad#dir = ".pad"

  " Bundle 'fmoralesc/vim-tutor-mode'

" use c-a in command mode
cnoremap <C-a> <Home>

" split/join
"nmap <leader>j :SplitjoinJoin<CR>
"nmap <leader>o :SplitjoinSplit<CR>

" git/svn blame - \g/\s on a visual block - FIXME svn blame not working
vmap <leader>g :<C-u>!git blame <C-r>=expand("%") <CR> \| sed -n <C-r>=line("'<") <CR>,<C-r>=line("'>") <CR>p <CR>
vmap <leader>s :<C-u>!svn blame <C-r>=expand("%") <CR> \| sed -n <C-r>=line("'<") <CR>,<C-r>=line("'>") <CR>p <CR>

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

" leader key bindings
map <leader>p :set paste!<CR>

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

" Use substitute() instead of printf() to handle '%%s' modeline in LaTeX
" files.
function! AppendModeline()
  let l:modeline = "// vim: ts=4:sw=4:sts=4:et"
  call append(line("$"), l:modeline)
endfunction
nnoremap <silent> <leader>ml :call AppendModeline()<CR>

" gundo
nmap <leader>u :GundoToggle<CR>

" taglist
nmap <leader>m :TlistToggle<CR>

" include path
set path=,,.,/usr/include,/usr/local/include

" idutils gid
" nmap <leader>g :call g:IDSearchCurrentWord()<CR>

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

" counterparts
nmap <SwipeUp> :split<CR>
nmap <SwipeDown> :split<CR><C-w>j<CR>
nmap <SwipeLeft> <C-w>v<CR>
nmap <SwipeRight> <C-w>v<CR><C-w>l<CR>
nmap <C-w>j :split<CR>

" ptag
nmap <leader>\ :ptag <C-r>=expand("<cword>")<CR><CR>

" remove highlighting
nmap <leader>\ :nohlsearch<CR>

" -fblocks
hi link cErrInParen Normal

" what
set modelines=10

" Append modeline after last line in buffer.
" Use substitute() instead of printf() to handle '%%s' modeline in LaTeX
" files.
function! AppendModeline()
  let l:modeline = "// vim: ts=4:sw=4:sts=4:et"
  call append(line("$"), l:modeline)
endfunction
nnoremap <silent> <leader>ml :call AppendModeline()<CR>

" clang
let g:clang_auto_select = 1
let g:clang_complete_auto = 1
let g:clang_complete_copen = 1
let g:clang_periodic_quickfix = 1
let g:clang_complete_snippets = 1
let g:clang_library_path = '/Applications/Xcode5-DP.app//Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib'
let g:clang_complete_macros = 1
let g:clang_complete_patterns = 1
"set updatetime=200
nmap <leader>c :call g:ClangUpdateQuickFix()<CR>

autocmd VimResized * wincmd=

"let g:ruby_path = "$HOME/.rvm/rubies/default/bin"

nmap <Leader>a :Tabularize /
vmap <Leader>a :Tabularize /
nmap <Leader>a= :Tabularize / = <CR>
vmap <Leader>a= :Tabularize / = <CR>
nmap <Leader>a: :Tabularize /:\zs<CR>
vmap <Leader>a: :Tabularize /:\zs<CR>

" tab hax
nmap <silent> <Leader>j :tabn<CR>
nmap <silent> <Leader>k :tabp<CR>
nmap <silent> <Leader>n :tabnew<CR>
" swap tag stack pop with tabnew
"nmap <C-BSlash> :po<CR>
"nmap <SwipeUp> :tabp<CR>
"nmap <SwipeDown> :tabp<CR>
nmap <C-t> :enew<CR>
nmap <C-n> :bnext<CR>
nmap <C-p> :bprev<CR>
nmap <leader>q :bd<CR>

command! F :%!python -m json.tool
command! G :%!jshon
command! J :%!jq .

au FileType crontab set nobackup nowritebackup

noremap <leader>d :diffoff \| windo if &diff \| hide \| endif<CR>
