#!/bin/bash
set -e 
set -o pipefail

########################
# CPU Microbenchmark
########################

# Install dependencies
sudo apt-get install make texinfo gcc g++ xz-utils bzip2 build-essential tcl libjemalloc-dev maven htop cmake -y

# CPU Microbenchmark
git clone https://github.com/eembc/coremark-pro

cd coremark-pro
make TARGET=linux64 build -j$(ncores)
cd ..

########################
# Filesystem Microbenchmark
########################

# Installing iozone on the board is really slow:
# Therefore we download the cross compiled program from a remove destination

mkdir keystone-iozone
cd keystone-iozone
wget -O iozone https://github.com/epfl-dcsl/miralis-benchmark/raw/refs/heads/main/iozone
chmod 777 iozone
cd ..

# git clone https://github.com/keystone-enclave/keystone-iozone
# cd keystone-iozone
# 
# git clone https://github.com/richfelker/musl-cross-make
# cd musl-cross-make
# 
# TARGET=riscv64-linux-musl make -j$(nproc) 
# TARGET=riscv64-linux-musl make install 
# 
# cd ..
# 
# git checkout 1378a4fb920e8177a2293c4600ab494ab51de6b8
# CCRV=musl-cross-make/output/bin/riscv64-linux-musl-gcc make keystone
# 
# cd ..

########################
# Network Microbenchmark
########################

sudo apt install automake autoconf texinfo -y

git clone https://github.com/HewlettPackard/netperf/
cd netperf

# Replace confiuration files - files are too old for riscv
rm config.guess
rm config.sub
wget -O config.guess http://git.savannah.gnu.org/cgit/config.git/plain/config.guess
wget -O config.sub http://git.savannah.gnu.org/cgit/config.git/plain/config.sub

# Install
./autogen.sh
./configure
make CFLAGS="-fcommon"
sudo make install

cd ..

########################
# SSH server
########################

# Install the ssh client for the experiments
sudo apt-get install openssh-server -y
sudo systemctl enable ssh
sudo systemctl start ssh

########################
# Memcached
########################

# Install memcached
sudo apt-get install memcached -y

# Change configuration to receive connections from the outside world
sudo sed -i 's/^-l\s*.*/-l 0.0.0.0/' /etc/memcached.conf

sudo systemctl start memcached
sudo systemctl enable memcached

########################
# Redis
########################

# Install redis
# wget https://download.redis.io/redis-stable.tar.gz
# tar -xzvf redis-stable.tar.gz
# cd redis-stable 
# make -j$(nproc)
# sudo make install
# cd ..

sudo apt install redis-server

# Remove the protected mode
redis-cli CONFIG SET protected-mode no

# Change configuration to receive connections from the outside world
sudo sed -i 's/127\.0\.0\.1/0.0.0.0/g' /etc/redis/redis.conf

sudo systemctl restart redis

# Install the sampler
git clone http://github.com/brianfrankcooper/YCSB.git

# Finally give the access right to the other scripts for ssh measurements
chmod 777 microbenchmark_cpu.sh
chmod 777 microbenchmark_fs.sh
chmod 777 microbenchmark_network.sh

########################
# Mysql
########################

# Install mysql
sudo apt install mysql-server -y
sudo systemctl start mysql
sudo systemctl enable mysql

# Alter the configuration file to receive connections from the outside world
sudo sed -i 's/^bind-address\s*=.*/bind-address = 0.0.0.0/' /etc/mysql/mysql.conf.d/mysqld.cnf
sudo sed -i 's/^mysqlx-bind-address\s*=.*/mysqlx-bind-address = 0.0.0.0/' /etc/mysql/mysql.conf.d/mysqld.cnf

sudo systemctl restart mysql

# Setup new user and database in mysql
sudo mysql -e "CREATE USER 'user'@'%' IDENTIFIED BY 'user';"
sudo mysql -e "GRANT ALL PRIVILEGES ON *.* TO 'user'@'%';"
sudo mysql -e "FLUSH PRIVILEGES;"
sudo mysql -e "CREATE DATABASE sbtest;"

# Restart mysql (now ready for the benchmarks)
sudo systemctl restart mysql

########################
# Firewall
########################

sudo apt install ufw -y
sudo systemctl enable ufw   
sudo systemctl start ufw  

sudo ufw allow ssh
# Redis
sudo ufw allow 6379
# Netperf
sudo ufw allow 12865
# Mysql
sudo ufw allow 3306
# Memcached
sudo ufw allow 11211
sudo systemctl restart ufw


########################
# Cornell box
########################

git clone https://github.com/RayTracing/raytracing.github.io/
cd raytracing.github.io/ 

cmake -B build/Release -DCMAKE_BUILD_TYPE=Release
cmake --build build/Release

cd ..

########################
# LLM inference
########################

git clone https://github.com/ggerganov/llama.cpp
cd llama.cpp
cmake -B build
cmake --build build --config Release
cd ..

wget https://huggingface.co/unsloth/DeepSeek-R1-Distill-Qwen-1.5B-GGUF/resolve/main/DeepSeek-R1-Distill-Qwen-1.5B-Q2_K.gguf?download=true
mv DeepSeek-R1-Distill-Qwen-1.5B-Q2_K.gguf?download=true model.gguf