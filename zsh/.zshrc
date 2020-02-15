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

## >>> conda initialize >>>
## !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/Users/cor/miniconda3/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
    conda deactivate
else
    if [ -f "/Users/cor/miniconda3/etc/profile.d/conda.sh" ]; then
        . "/Users/cor/miniconda3/etc/profile.d/conda.sh"
    else
        export PATH="/Users/cor/miniconda3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<

POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=(virtualenv anaconda context dir vcs)

# Allows to install pip in global
gpip() {
    PIP_REQUIRE_VIRTUALENV = "" pip3 "$@"
}

# Increase number of open Jupyter notebooks
ulimit -n 4096

#tmux
#bindkey "^[[200~" pbpaste
bindkey '^[[200~' bracketed-paste-magic

# load rbenv automatically
export PATH="$HOME/.rbenv/bin:$PATH"
eval "$(rbenv init -)"

