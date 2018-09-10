apt-get -y install vim /
	redshift \
    stow \
    scrot \
    xclip \
    zathura \
    curl \
    i3 \
    i3status \
    i3lock \
    dmenu \
    xbacklight \
    gnome-settings-daemon

pip3 install --no-cache-dir flake8

curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
        https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
