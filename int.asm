section .text
global _start

; The int to print is stored in rax

print2:
    mov rsi, print_buf
    add rax, 30h         ; 0x30 ('0')
    mov [rsi], rax
    inc rsi
    mov [rsi], rax
    inc rsi
    mov [rsi], rax
    inc rsi
    mov [rsi], rax
    inc rsi
    mov byte [rsi], 10
    mov     rax, 1
    mov     rdi, 1
    mov     rsi, print_buf
    mov     rdx, 5
    syscall
    ret

print_hex_uint:
    xor rdx, rdx               ; The number of characters in the buffer
    mov rsi, print_buf         ; si points to the target buffer
    mov rbx, rax               ; store a copy in bx

    cmp rax, 0
    jne  start_strip_leading_zeros
    ; Special case for 0
    mov byte [rsi], 030h
    inc rsi
    inc rdx
    jmp done_conversion

strip_leading_zeros:
    shl rbx, 4                      ; get the next part

start_strip_leading_zeros:
    mov rax, rbx          ; load the number into ax
    shr rax, 60           ; grap the last byte
    jz strip_leading_zeros ; if this is zero then keep stripping. Otherwise continue to printing below

convert_loop:
    mov rax, rbx          ; load the number into ax
    shr rax, 60           ; grab the last byte

    cmp rax, 9h          ; check what we should add
    jg  greater_than_9
    add rax, 30h         ; 0x30 ('0')
    jmp converted

greater_than_9:
    ; num + 'a' - 10
    add rax, 61h         ; or 0x61 ('a')
    sub rax, 10

converted:
    mov [rsi], rax
    inc rsi
    inc rdx
    shl rbx, 4           ; get the next part
    jnz convert_loop

done_conversion:
    mov byte [rsi], 10 ; new line
    inc rdx

    mov     rax, 1
    mov     rdi, 1
    mov     rsi, print_buf
    syscall
    ret

_start:
        mov     rax, 0
        call    print_hex_uint
        mov     rax, 12h
        call    print_hex_uint
        mov     rax, 60
        xor     rdi, rdi
        syscall

section .bss
        print_buf: resb 8

