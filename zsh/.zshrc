# Path to your oh-my-zsh installation.
export ZSH="$HOME/.oh-my-zsh"

ZSH_THEME="gnzh"

plugins=(
	git
	extract
)

source $ZSH/oh-my-zsh.sh

# Issue with backspace not working
# https://askubuntu.com/questions/54145/how-to-fix-strange-backspace-behaviour-with-urxvt-zsh
# https://github.com/thestinger/termite/issues/123
TERM=xterm

# History up-down arrow history search
autoload -U up-line-or-beginning-search
autoload -U down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search
bindkey "^[[A" up-line-or-beginning-search # Up
bindkey "^[[B" down-line-or-beginning-search # Down
