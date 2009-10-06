set nocompatible
syntax on
set ignorecase hlsearch incsearch smartcase
" Disable screen clearing
set t_te=
set autoindent expandtab tabstop=4 shiftwidth=4
filetype plugin indent on
if has('mouse')
  set mouse=a
endif
set t_Co=256            " set 256 color
set clipboard+=unnamed  " yank and copy to X clipboard

