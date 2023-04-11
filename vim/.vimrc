" Install vim-plug if  it's not already installed
if empty(glob('~/.vim/autoload/plug.vim'))
	silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
		\ https://raw.github.com/junegunn/vim-plug/master/plug.vim
	autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

" Get solarized color theme
if empty(glob('~/.vim/colors/solarized.vim'))
    silent !curl -fLo ~/.vim/colors/solarized.vim --create-dirs
        \ https://raw.githubusercontent.com/altercation/vim-colors-solarized/master/colors/solarized.vim
endif

call plug#begin()
    if has('python') || has('python3')
		Plug 'SirVer/ultisnips'					" Snippets
	endif
	Plug 'tpope/vim-fugitive'					" git integration
	Plug 'christoomey/vim-tmux-navigator'       " tmux integration
	Plug 'michaeljsmith/vim-indent-object'
	Plug 'tpope/vim-git'                        " More git stuff
	Plug 'sheerun/vim-polyglot'
	Plug 'altercation/vim-colors-solarized'		" Solarized color scheme
	Plug 'ervandew/supertab'                    " Make tab work with multiple packages
	Plug 'tpope/vim-dispatch'                   " Run stuff asynchrously
	Plug 'janko/vim-test'                       " Run tests
	Plug 'troydm/zoomwintab.vim'
	Plug 'dense-analysis/ale'                   " Linting
	Plug 'tpope/vim-unimpaired'					" Smart hot-keys with [ and ]
	" Plug 'ctrlpvim/ctrlp.vim'				    " Fuzzy matching for files
	Plug 'vim-airline/vim-airline'				" Airline status at bottom
	Plug 'vim-airline/vim-airline-themes'       " Theme for airline
	Plug 'preservim/nerdtree'                   " Directory explorer
	Plug 'wellle/targets.vim'					" Advanced targets selecting
	Plug 'tmhedberg/SimpylFold'					" Simple folding for python
	Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }   " fzf is a general-purpose command-line fuzzy finder.
	Plug 'junegunn/fzf.vim'                     " Fzf vim commands

    if v:version > 800 && (has('python') || has('python3'))
		let g:plug_timeout = 300	" Increase vim-plug timeout for YouCompleteMe
		Plug 'Valloric/YouCompleteMe'			" tab completion stuff
	endif
call plug#end()

" Map the leader key to space
let mapleader=' '

" YCM settings
let g:SuperTabDefaultCompletionType = '<C-n>'
let g:ycm_key_list_select_completion = ['<C-n>', '<Down>']
let g:ycm_key_list_previous_completion = ['<C-p>', '<Up>']
let g:ycm_auto_trigger = 0
let g:ycm_key_invoke_completion = '<tab>'
let g:ycm_min_num_identifier_candidate_chars = 3
let g:ycm_min_num_of_chars_for_completion = 3
let g:ycm_max_num_candidates = 10
noremap <leader>] :YcmCompleter GoTo<cr>
noremap <leader>yd :YcmCompleter GetDoc<cr>
:set completeopt="menu"

" Jump to the next field
let g:UltiSnipsExpandTrigger="<tab>" " better key bindings for UltiSnipsExpandTrigger
let g:UltiSnipsJumpForwardTrigger="<tab>"
let g:UltiSnipsJumpBackwardTrigger="<s-tab>"
let g:UltiSnipsEditSplit="vertical"
let g:UltiSnipsSnippetDirectories=[$HOME."/dotfiles/snippets"]

" make test commands execute using dispatch.vim
let test#strategy = "dispatch"

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
	au BufRead,BufNewFile *.{md} setl tw=79 spell  tabstop=4 softtabstop=4 expandtab smarttab shiftwidth=4
	au BufRead,BufNewFile *.{yml} setl number tabstop=2 softtabstop=2 expandtab smarttab shiftwidth=2 indentkeys-=<:> ruler
	au BufRead,BufNewFile *.{html} setl tabstop=2 softtabstop=2 expandtab smarttab shiftwidth=2
	au BufRead,BufNewFile *.{json} setl tabstop=2 softtabstop=2 expandtab smarttab shiftwidth=2
	au BufRead,BufNewFile *.{tf} setl tabstop=2 softtabstop=2 expandtab smarttab shiftwidth=2
	au BufRead,BufNewFile make setl noexpandtab
	au BufRead,BufNewFile * setl spell tabstop=4 softtabstop=4 shiftwidth=4
	au BufRead,BufNewFile *.{sh} setl tabstop=2 softtabstop=2 expandtab smarttab shiftwidth=2

	" Yaml settings
	au! BufNewFile,BufReadPost *.{yaml,yml} set filetype=yaml
	autocmd FileType yaml setlocal ts=2 sts=2 sw=2 expandtab

	" Automatically remove trailing white spaces
	au BufWritePre *.py %s/\s\+$//e
	au BufWritePre *.yml %s/\s\+$//e
	au BufWritePre *.sh %s/\s\+$//e

	" Regenerate tags when saving Python files
	"au BufWritePost *.py silent! !ctags -R 2> /dev/null &

	" Set scripts to be executable from the shell
	"au BufWritePost * if getline(1) =~ "^#!" | silent !chmod +x % | endif

	" Source vimrc after writing to it
	au BufwRitePost ~/.vimrc source ~/.vimrc

	" Jump between methods, in python
	autocmd FileType python nnoremap <buffer> [[ ?^class\\|^\s*def<CR>
	autocmd FileType python nnoremap <buffer> ]] /^class\\|^\s*def<CR>
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
nnoremap <space>gs :Git<CR>
nnoremap <space>gp :Git push<CR>
nnoremap <space>gd :Gdiff<CR>

" Save with `w!!` when 'readonly' is set
cnoremap w!! execute 'silent! write !sudo tee % >/dev/null' <bar> edit!

" Shortcut to rapidly toggle `set list`
nmap <leader>l :set list!<CR>

" Use the same symbols as TextMate for tabstops and EOLs
set listchars=tab:▸\ ,eol:¬

" Fast edit vimrc
map <leader>e :e! ~/dotfiles/vim/.vimrc<cr>

" Vim-test
nnoremap <Leader>tn :TestNearest '-sqq'<CR>
nnoremap <Leader>tf :TestFile<CR>
nnoremap <Leader>ts :TestSuite<CR>
nnoremap <Leader>tl :TestLast<CR>
nnoremap <Leader>tv :TestVisit<CR>

" ZoomWin
nnoremap <silent><C-w>z :ZoomWinTabToggle<CR>

" ALE
nnoremap <F7> :ALEToggle<CR>
let g:ale_linters = {
\  'python': ['flake8', 'mypy'],
\}
let g:ale_fixers = {
\   '*': ['remove_trailing_lines', 'trim_whitespace'],
\   'python': ['black'],
\}
nnoremap <Leader>lo :lopen<CR>
nnoremap <Leader>lc :lclose<CR>
nnoremap <Leader>ad :ALEDisable<CR>
nnoremap <Leader>ae :ALEEnable<CR>
nnoremap <Leader>af :ALEFix<CR>
let g:ale_python_black_options='-l 79'

" Maintain undo history between sessions
if !isdirectory($HOME."/.vim/undodir")
	call mkdir($HOME."/.vim/undodir", "p")
endif
set undofile
set undodir=~/.vim/undodir

" Nerdtree
nnoremap <Leader>nt :NERDTreeToggle<CR>

"Invisible character colors
highlight NonText ctermfg=2
highlight SpecialKey ctermfg=2

" Move when buffers are hidden
set hidden

" Enable project specific vimrc
set exrc

" Set backpspace
" https://vi.stackexchange.com/questions/2162/why-doesnt-the-backspace-key-work-in-insert-mode#2163
set backspace=indent,eol,start

" Tags
set tags=tags
set notagbsearch      " solves tags file not sorted error

" Clipboard
set clipboard=unnamed

" Airline settings
let g:airline_theme='solarized'
let g:airline_solarized_bg='dark'

" Ignore for CtrlP
set wildignore+=*/.git/*,*/venv/*,*/*.egg-info/*

" Black settings
let g:black_linelength=79

" Move visual selection
vnoremap J :m '>+1<cr>gv=gv
vnoremap K :m '<-2<cr>gv=gv

" Reload current file
nnoremap <F5> :e %<CR>

" Simply fold settings
let g:SimpylFold_docstring_preview=1
let g:SimpylFold_fold_docstring=0
let g:SimpylFold_fold_import=0

" Fzf
let g:fzf_layout = { 'down': '20%' }
nnoremap <leader>fg :GFiles<CR>
nnoremap <leader>ff :Files<CR>
nnoremap <leader>fc :Colors<CR>
nnoremap <leader>fb :Buffers<CR>
nnoremap <leader>fl :Lines<CR>
nnoremap <leader>fh :History<CR>
set rtp+=/opt/homebrew/opt/fzf

