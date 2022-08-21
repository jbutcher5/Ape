#!/usr/bin/env bash

set -xe
cargo run
yasm out.asm -f elf64 -g dwarf2
ld out.o
./a.out
echo "EXIT CODE:"
echo $?
