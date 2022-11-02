#!/usr/bin/env sh

set -xe
cargo run

nasm out.asm -f elf64 -g

clang out.o stdlib.c -no-pie -g
./a.out

gcc out.o stdlib.c -no-pie -g
./a.out
