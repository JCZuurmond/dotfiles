# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# Path to your oh-my-zsh installation.
export TERM="xterm-256color"
export ZSH="$HOME/.oh-my-zsh"

ZSH_THEME="powerlevel10k/powerlevel10k"

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

POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=(virtualenv anaconda status dir vcs)
POWERLEVEL9K_RIGHT_PROMPT_ELEMENTS=()

# Allows to install pip in global
gpip() {
    PIP_REQUIRE_VIRTUALENV="" pip3 "$@"
}

# Increase number of open Jupyter notebooks
ulimit -n 4096

#tmux
#bindkey "^[[200~" pbpaste
bindkey '^[[200~' bracketed-paste-magic

# load rbenv automatically
export PATH="$HOME/.rbenv/bin:$PATH"
eval "$(rbenv init -)"

# Virtual env wrapper
export WORKON_HOME=$HOME/.virtualenvs
export VIRTUALENVWRAPPER_PYTHON=/usr/local/bin/python3
source /usr/local/bin/virtualenvwrapper.sh
export PYENV_VIRTUALENVWRAPPER_PREFER_PYVENV="true"

# Weather
alias weather-amsterdam='curl v2.wttr.in/Amsterdam'

# Opt-out of Azure functions telemetry
FUNCTIONS_CORE_TOOLS_TELEMETRY_OPTOUT=1

# Direnv
eval "$(direnv hook zsh)"

# pyenv
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
if command -v pyenv 1>/dev/null 2>&1; then
  eval "$(pyenv init -)"
  eval "$(pyenv virtualenv-init -)"
fi

autoload -U +X bashcompinit && bashcompinit
complete -o nospace -C /usr/local/bin/terraform terraform

# Go settings
export GOROOT=/usr/local/opt/go/libexec
export PATH=$PATH:$GOROOT/bin

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
# Opt-out of Azure functions telmeterty
export FUNCTIONS_CORE_TOOLS_TELEMETRY_OPTOUT=1

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/Applications/google-cloud-sdk/path.zsh.inc' ]; then . '/Applications/google-cloud-sdk/path.zsh.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/Applications/google-cloud-sdk/completion.zsh.inc' ]; then . '/Applications/google-cloud-sdk/completion.zsh.inc'; fi

# Emacs
export PATH="$PATH:/Users/cor/.emacs.d/bin"

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
