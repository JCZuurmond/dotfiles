apt-get update 
apt-get install -y \
  wget \
  tar \
  build-essential \
  libgcrypt11-dev \
  zlib1g-dev \
  checkinstall \
  libreadline-gplv2-dev \
  libncursesw5-dev \
  libssl-dev \
  libsqlite3-dev \
  tk-dev \
  libgdbm-dev \
  libc6-dev \
  libbz2-dev

wget https://www.python.org/ftp/python/3.6.3/Python-3.6.3.tar.xz
tar xJf Python-3.6.3.tar.xz 
cd Python-3.6.3
./configure
make
make install
