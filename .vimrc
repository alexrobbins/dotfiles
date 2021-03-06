colorscheme desert

syntax on
filetype plugin indent on
set expandtab
set hls "Highlight search terms
set incsearch
set shiftwidth=2
set softtabstop=2
set tabstop=2
set tags=~/prj/tags
set wildmode=list:longest,full
set wildmenu
set cindent
set cmdheight=2
setlocal smarttab
setlocal textwidth=80

set wrap linebreak textwidth=0

let g:netrw_list_hide = "\.pyc,\.swp,\.git"

" Settings for VimClojure
let vimclojure#HighlightBuiltins=1      " Highlight Clojure's builtins
let vimclojure#ParenRainbow=1           " Rainbow parentheses
" let vimclojure#WantNailgun=1            " Interactive mode with bonus features
let vimclojure#DynamicHighlighting=1    " Highlight using symbols from active
                                        " namespaces

" set current working dir to working dir of current buffer's file
map ,d :cd %:h<CR>

set clipboard=unnamed

if &term =~ '^screen'
    " tmux will send xterm-style keys when its xterm-keys option is on
    execute "set <xUp>=\e[1;*A"
    execute "set <xDown>=\e[1;*B"
    execute "set <xRight>=\e[1;*C"
    execute "set <xLeft>=\e[1;*D"
endif
