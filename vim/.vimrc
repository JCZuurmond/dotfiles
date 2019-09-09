" Install vim-plug if  it's not already installed
if empty(glob('~/.vim/autoload/plug.vim'))
	silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
		\ https://raw.github.com/junegunn/vim-plug/master/plug.vim
	autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin()
	Plug 'SirVer/ultisnips'
	Plug 'nvie/vim-flake8'
	Plug 'terryma/vim-multiple-cursors'
	Plug 'tpope/vim-fugitive'
	Plug 'christoomey/vim-tmux-navigator'
	Plug 'alfredodeza/pytest.vim'
	Plug 'scrooloose/nerdtree'
	Plug 'Xuyuanp/nerdtree-git-plugin'
	Plug 'michaeljsmith/vim-indent-object'
	Plug 'tpope/vim-git'
	Plug 'sheerun/vim-polyglot'
	Plug 'trevordmiller/nova-vim'
	Plug 'altercation/vim-colors-solarized'
	Plug 'ervandew/supertab'

	let g:plug_timeout = 300	" Increase vim-plug timeout for YouCompleteMe
	Plug 'Valloric/YouCompleteMe'
call plug#end()

" Make YCM compatible with UltiSnips (using supertab)
let g:ycm_key_list_select_completion = ['<C-n>', '<Down>']
let g:ycm_key_list_previous_completion = ['<C-p>', '<Up>']
let g:SuperTabDefaultCompletionType = '<C-n>'

" Jump to the next field
let g:UltiSnipsExpandTrigger="<tab>" " better key bindings for UltiSnipsExpandTrigger
let g:UltiSnipsJumpForwardTrigger="<tab>"
let g:UltiSnipsJumpBackwardTrigger="<s-tab>"
let g:UltiSnipsEditSplit="vertical"
let g:UltiSnipsSnippetDirectories=[$HOME."/dotfiles/snippets"]

"set color scheme to nova
syntax on
set background=dark
let g:solarized_termcolors=16
let g:solarized_contrast="high"
colorscheme solarized

" Settings for multiple cursors
let g:multi_cursor_exit_from_insert_mode=0

if has("autocmd")
	" File settings
	au BufRead,BufNewFile *.{py} setl number tw=79 tabstop=4 softtabstop=4 expandtab smarttab shiftwidth=4 indentkeys-=<:> ruler
	au BufRead,BufNewFile *.{snippets} setl spell tabstop=4 softtabstop=4 smarttab shiftwidth=4
	au BufRead,BufNewFile *.{md} setl tw=79 spell 
	au BufRead,BufNewFile *.{html} setl tabstop=2 softtabstop=2 expandtab smarttab shiftwidth=2 
	au BufRead,BufNewFile make setl noexpandtab 
	au BufRead,BufNewFile * setl spell tabstop=4 softtabstop=4 shiftwidth=4 tw=79

	" Automatically remove trailing white spaces
	au BufWritePre *.py %s/\s\+$//e

	" Regenerate tags when saving Python files
	au BufWritePost *.py silent! !ctags -R &

	" Set scripts to be executable from the shell
	au BufWritePost * if getline(1) =~ "^#!" | silent !chmod +x % | endif
endif

"Practice to not use arrows
nnoremap <Left> :echo "No left for you!"<CR>
vnoremap <Left> :<C-u>echo "No left for you!"<CR>
inoremap <Left> <C-o>:echo "No left for you!"<CR>
nnoremap <Right> :echo "No right for you!"<CR>
vnoremap <Right> :<C-u>echo "No right for you!"<CR>
inoremap <Right> <C-o>:echo "No right for you!"<CR>
nnoremap <Up> :echo "No up for you!"<CR>
vnoremap <Up> :<C-u>echo "No up for you!"<CR>
inoremap <Up> <C-o>:echo "No up for you!"<CR>
nnoremap <Down> :echo "No down for you!"<CR>
vnoremap <Down> :<C-u>echo "No down for you!"<CR>
inoremap <Down> <C-o>:echo "No down for you!"<CR>

" disables error sounds
set novisualbell

" Map capitals to lower case
:command! WQ wq
:command! Wq wq
:command! W w
:command! Q q

" Rest of line to next line enter
map <F2> i<CR><ESC>

" Set docstring guide to Numpy 
let g:ultisnips_python_style="numpy"
let g:ultisnips_python_quoting_style="double"

" Toggle paste modus
set pastetoggle=<F3>

" Vertical split is preferred
set diffopt+=vertical

" Short cuts for git (vim fugititve)
nnoremap <space>gs :Gstatus<CR>
nnoremap <space>gp :Gpush<CR>
nnoremap <space>gd :Gdiff<CR>
nnoremap <space>gw :w<CR>:Gcommit<CR>i

" Save with `w!!` when 'readonly' is set
cnoremap w!! execute 'silent! write !sudo tee % >/dev/null' <bar> edit!

" Shortcut to rapidly toggle `set list`
nmap <leader>l :set list!<CR>

" Use the same symbols as TextMate for tabstops and EOLs
set listchars=tab:▸\ ,eol:¬

" Pytest
nmap <silent><Leader>tg :Pytest file<CR>
nmap <silent><Leader>tf :Pytest function<CR>
nmap <silent><Leader>tc :Pytest class<CR>
nmap <silent><Leader>tm :Pytest method<CR>

" Maintain undo history between sessions
if !isdirectory($HOME."/.vim/undodir")
	call mkdir($HOME."/.vim/undodir", "p")
endif
set undofile
set undodir=~/.vim/undodir

"Invisible character colors 
highlight NonText ctermfg=2
highlight SpecialKey ctermfg=2

" Move when buffers are hidden
set hidden

" NerdTree
let NERDTreeShowHidden=1
nnoremap <Leader>nt :NERDTree<CR>

" Enable project specific vimrc
set exrc

" Set backpspace
" https://vi.stackexchange.com/questions/2162/why-doesnt-the-backspace-key-work-in-insert-mode#2163
set backspace=indent,eol,start

" Map the leader key to comma
let mapleader = ','

" Look for a tags file recursively in the parent directory
set tags=tags  
