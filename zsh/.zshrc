# Path to your oh-my-zsh installation.
export ZSH="$HOME/.oh-my-zsh"

ZSH_THEME="gnzh"

plugins=(
	git
	archive
	extract
)

source $ZSH/oh-my-zsh.sh

# Issue with backspace not working
# https://askubuntu.com/questions/54145/how-to-fix-strange-backspace-behaviour-with-urxvt-zsh
# https://github.com/thestinger/termite/issues/123
TERM=xterm
