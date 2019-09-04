# Path to your oh-my-zsh installation.
export TERM="xterm-256color"
export ZSH="$HOME/.oh-my-zsh"

ZSH_THEME="powerlevel9k/powerlevel9k"

plugins=(
	git
	extract
)

source $ZSH/oh-my-zsh.sh

# Issue with backspace not working
# https://askubuntu.com/questions/54145/how-to-fix-strange-backspace-behaviour-with-urxvt-zsh
# https://github.com/thestinger/termite/issues/123
#TERM=xterm

# History up-down arrow history search
autoload -U up-line-or-beginning-search
autoload -U down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search
bindkey "^[[A" up-line-or-beginning-search # Up
bindkey "^[[B" down-line-or-beginning-search # Down

# Set nova color scheme with
# https://github.com/trevordmiller/nova-tmux/blob/master/assets/.tmux.conf
set -ga terminal-overrides ",xterm-256color:Tc"
set -g default-terminal "xterm-256color"
set -g status-style 'bg=#1E272C,fg=#6A7D89'
set -g window-status-current-style 'fg=cyan'
set -g mode-style 'bg=cyan,fg=black'
set -g message-style 'bg=black,fg=cyan'
set -g pane-border-style 'bg=black,fg=#1E272C'
set -g pane-active-border-style 'bg=black,fg=#1E272C'
