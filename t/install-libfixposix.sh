#!/bin/sh

curl -L https://github.com/sionescu/libfixposix/archive/v0.3.0.tar.gz | tar xzf -

cd libfixposix-0.3.0
autoreconf -i -f

mkdir build/
cd build/
../configure --prefix=$HOME/libfixposix
make
make install
