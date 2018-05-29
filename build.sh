#!/bin/sh

DIR=$1

eval `opam config env`
make clean
make
