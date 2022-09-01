#!/usr/bin/env bash

set -xe
cargo run
nasm out.asm -f elf64
gcc out.o -no-pie
./a.out
echo "EXIT CODE:"
echo $?
