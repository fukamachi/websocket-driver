#!/bin/sh

curl -L https://github.com/libuv/libuv/archive/v1.6.1.tar.gz | tar xzf -

cd libuv-1.6.1
./autogen.sh
./configure --prefix=$HOME/libuv
make
make install
