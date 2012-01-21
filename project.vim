set autowrite
set encoding=UTF-8
set makeprg=cabal\ build
"set efm=%E\ %#[error]\ %f:%l:\ %m,%C\ %#[error]\ %p^,%C\ %#[error]\ \ %m,%-C%.%#,%Z,
"       \%W\ %#[warn]\ %f:%l:\ %m,%C\ %#[warn]\ %p^,%-C%.%#,%Z,
"       \%-G%.%#
set ofu=syntaxcomplete#Complete
cd .
set path=./**

au BufWritePost *.hs silent! !find src -name \*.\*hs | xargs hasktags -c -x -f ./tags &
set tags=tags
e .

