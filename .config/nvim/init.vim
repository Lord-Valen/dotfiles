let g:vim_home = get(g:, 'vim_home', expand('~/.config/nvim/'))
let config_list = [
        \ 'plug.vim',
        \ 'opti.vim',
        \ 'maps.vim',
        \ 'auto.vim',
        \ 'gvim.vim' 
        \ ]
for file in config_list
                exec 'source ' g:vim_home.file
endfor
