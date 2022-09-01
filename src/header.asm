section .text
global main 
extern printf

print_value:
    mov rsi, rdi
    mov rdi, as_int
    mov rax, 0
    call printf
    ret
