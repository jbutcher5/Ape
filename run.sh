#!/usr/bin/env sh

set -xe
cargo run
nasm out.asm -f elf64 -g
gcc out.o -no-pie -g
./a.out
