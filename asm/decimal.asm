section .text
global _start

; Takes a uint64 in rax and prints the result using the message buffer
print_decimal_uint64:
    xor r8, r8 ; The number of chars in the buffer
    mov rsi, print_buf         ; Set si to the start of the buffer
    mov rbx, rax               ; store a copy in bx

; Increment until we have taken enough space to store the entire integer.
; Do this by dividing by 10 until the result is available;
increment_message_pointer:
    mov rcx, 10 ; Cannot div by a constant.
    mov rdx, 0  ; We do not need the extra precision of rdx.
    div rcx

    inc rsi
    inc r8
    ; We are done if the num is now 0
    cmp rax, 0
    jne increment_message_pointer

fill_buffer_uint64:
    ; If we reach here, we have rsi pointing to the amount we need for the num and 
    ; rax == 0.
    ; Store the newline.
    inc r8
    mov byte [rsi], 10 ; \n
    dec rsi

    ; We need the number again
    mov rax, rbx

add_one_char_uint64:
    mov rcx, 10 ; Cannot div by a constant.
    mov rdx, 0  ; We do not need the extra precision of rdx.
    div rcx

    ; rdx stores the remainer of the devision.
    add rdx, 30h         ; 0x30 ('0')
    ; dl 8 bit portion of rdx
    mov byte [rsi], dl
    dec rsi

    ; We are done if the result is 0
    cmp rax, 0
    jne add_one_char_uint64

; Call write with buffer already filled.
do_print_decimal_uint64:
    mov     rax, 1
    mov     rdi, 1
    mov     rdx, 7
    mov     rsi, print_buf
    syscall
    ret

_start:
        mov     rax, 0
        call    print_decimal_uint64
        mov     rax, 123
        call    print_decimal_uint64
        mov     rax, 60
        xor     rdi, rdi
        syscall

section .bss
        print_buf: resb 20

