section .text
global _start

; inputs
; ------
; rax: uint64 to format
; rcx: base
print_uint64:
    mov rsi, print_buf         ; Set si to the start of the buffer
    call format_uint64
    mov rsi, print_buf         ; format_uint64 can change rsi

    ; Add the newline at the end of the buf
    mov rbx, rdx
    add rbx, rsi
    mov byte [rbx], 10
    inc rdx
    ; Call write syscall
    mov     rax, 1 ; write syscall
    mov     rdi, 1 ; stdout
    mov     rsi, print_buf
    syscall

    ret

; inputs
; ------
; rax: uint64 to format
; rcx: base
; rsi: buffer pointer - must be big enough. Not bounds checked.
;      Might be changed through execution.
;
; returns
; ------
; rdx: The size of the buffer - no newline.
format_uint64:
    xor r8, r8 ; The number of chars in the buffer
    mov rbx, rax               ; store a copy in bx

; Increment until we have taken enough space to store the entire integer.
; Do this by dividing by 10 until the result is available;
increment_message_pointer:
    mov rdx, 0  ; We do not need the extra precision of rdx.
    div rcx

    inc rsi
    inc r8
    ; We are done if the num is now 0
    cmp rax, 0
    jne increment_message_pointer

fill_buffer_uint64:
    dec rsi

    ; We need the number again
    mov rax, rbx

add_one_char_uint64:
    mov rdx, 0  ; We do not need the extra precision of rdx.
    div rcx ; rcx has the base

    cmp rcx, 16
    je base_selection_hex

base_selection_decimal:
    call format_char_decimal
    jmp base_selection_end

base_selection_hex:
    call format_char_hex
    jmp base_selection_end

base_selection_end:

    dec rsi

    ; We are done if the result is 0
    cmp rax, 0
    jne add_one_char_uint64

; We are done so return the result
    mov     rdx, r8
    ret

; Takes value in rdx and inserts decimal char into buffer at rsi
; Works for all bases < 10
format_char_decimal:
    ; rdx stores the remainder of the devision.
    add rdx, 30h         ; 0x30 ('0')
    ; dl 8 bit portion of rdx
    mov byte [rsi], dl
    ret

; Takes value in rdx and inserts hex char into buffer at rsi
format_char_hex:
    cmp rdx, 9
    jg format_char_hex_greater_than_9
    call format_char_decimal
    jmp format_char_hex_end

format_char_hex_greater_than_9:
    ; num + 'a' - 10
    add rdx, 61h         ; or 0x61 ('a')
    sub rdx, 10
    mov byte [rsi], dl

format_char_hex_end:
    ret

_start:
        mov     rax, 381
        mov     rcx, 10
        call    print_uint64
        mov     rax, 381
        mov     rcx, 16
        call    print_uint64
        mov     rax, 381
        mov     rcx, 2
        call    print_uint64
        mov     rax, 60
        xor     rdi, rdi
        syscall

section .bss
        print_buf: resb 65 ; uint64 in binary plus new line.

