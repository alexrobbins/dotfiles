colorscheme desert

set expandtab
set hls "Highlight search terms
set incsearch
set shiftwidth=4
set softtabstop=4
set tabstop=4
setlocal smartindent
setlocal smarttab
setlocal textwidth=80

set wrap linebreak textwidth=0

let g:netrw_list_hide = "\.pyc,\.swp,\.git"

" Settings for VimClojure
let vimclojure#HighlightBuiltins=1      " Highlight Clojure's builtins
let vimclojure#ParenRainbow=1           " Rainbow parentheses
let vimclojure#WantNailgun=1            " Interactive mode with bonus features
let vimclojure#DynamicHighlighting=1    " Highlight using symbols from active
                                        " namespaces
