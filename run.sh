#!/usr/bin/env sh

set -xe
cargo run
nasm out.asm -f elf64
gcc out.o -no-pie
./a.out
