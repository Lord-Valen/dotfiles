"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => General maps
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let mapleader = " "

cnoremap w!! execute 'silent! write !sudo tee % >/dev/null' <bar> edit!

" Remap ESC to ii
:imap ii <Esc>

" Smart home
function! SmartHome()
        let first_nonblank = match(getline('.'), '\S') + 1
        if first_nonblank == 0
                return col('.') + 1 >= col('$') ? '0' : '^'
        endif
        if col('.') == first_nonblank
                return '0'  " if at first nonblank, go to start line
        endif
        return &wrap && wincol() > 1 ? 'g^' : '^'
endfunction

noremap <expr> <silent> <Home> SmartHome()
imap <silent> <Home> <C-O><Home>

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Open terminal inside Vim
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
map <Leader>tt :vnew term://fish

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Splits and Tabbed Files
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Remap splits navigation to just CTRL + hjkl
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

" Make adjusing split sizes a bit more friendly
noremap <silent> <C-Left> :vertical resize +3<CR>
noremap <silent> <C-Right> :vertical resize -3<CR>
noremap <silent> <C-Up> :resize +3<CR>
noremap <silent> <C-Down> :resize -3<CR>

" Change 2 split windows from vert to horiz or horiz to vert
map <Leader>th <C-w>t<C-w>H
map <Leader>tk <C-w>t<C-w>K
