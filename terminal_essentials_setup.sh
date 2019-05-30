# These go through the essentials for working on a remote server in a terminal
# (through ssh)
# curl https://raw.githubusercontent.com/JCZuurmond/dotfiles/work/terminal_essentials_setup.sh | bash

# History search with and down keys
wget https://raw.githubusercontent.com/JCZuurmond/dotfiles/master/inputrc/.inputrc -O ~/.inputrc

# vim settings
wget https://raw.githubusercontent.com/JCZuurmond/dotfiles/master/vim/.vimrc -O ~/.vimrc

# UNIX CR-LF line endings
git config --global core.autocrlf false

# VIM plugin manager
curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
	https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

# Install all plugins
while read i; do git clone https://github.com/$i ~/.vim/plugged/$(basename $i); done < <(cat ~/.vimrc | grep Plug | cut -d"'" -f 2)

# solarized color scheme
mkdir ~/.vim/colors
wget https://raw.githubusercontent.com/altercation/vim-colors-solarized/master/colors/solarized.vim -O ~/.vim/colors/solarized.vim
