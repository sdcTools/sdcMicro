#!/bin/bash
g++  -DNDEBUG  -I/usr/local/include  -std=c++11 -fpic  -g -O2  -c recordSwap.cpp -o recordSwap.o
g++ -shared  -L/usr/local/lib -o recordSwapping.so recordSwap.o