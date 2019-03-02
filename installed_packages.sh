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
    i3blocks \
    dmenu \
    xbacklight \
    gnome-settings-daemon \
    xautolock \
    acpi \
    xdotools

pip3 install --no-cache-dir flake8

curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
        https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

# Python 3, check:
# http://ubuntuhandbook.org/index.php/2017/07/install-python-3-6-1-in-ubuntu-16-04-lts/

# Docker, check:
# https://www.digitalocean.com/community/tutorials/how-to-install-and-use-docker-on-ubuntu-16-04

# Pulseaudio-ctl (command line tool), check:
# https://github.com/graysky2/pulseaudio-ctl

# Rstudio, check(?):
# https://mikewilliamson.wordpress.com/2016/11/14/installing-r-studio-on-ubuntu-16-10/

# Termite:
# https://askubuntu.com/questions/739163/how-to-install-termite
