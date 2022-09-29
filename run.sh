#!/usr/bin/env sh

set -xe
cargo run

nasm out.asm -f elf64 -g

clang test.c out.o -no-pie -g
./a.out

gcc test.c out.o -no-pie -g
./a.out
