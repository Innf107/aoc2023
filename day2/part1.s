BITS 64
GLOBAL _start

; r15 ~ buffer index

; does not mutate rbx
read_char:
    cmp r15, [input_buffer_capacity]
    jl .skip_refill
    
    ; refill
    mov rax, 0 ; read
    mov rdi, [fd] ; input file descriptor
    mov rsi, input_buffer
    mov rdx, 1
    syscall

    cmp rax, 0
    je .on_eof

    mov [input_buffer_capacity], rax

    mov r15, 0 ; reset the buffer index

.skip_refill:
    mov rax, [input_buffer + r15]
    inc r15
    ret
.on_eof:
    ret


; returns in rbx
; reads one past the last digit (stored in rax)
read_number:
    mov rbx, 0
.read_loop:
    call read_char
    cmp rax, 48
    jl .read_done
    cmp rax, 57
    jg .read_done

    sub rax, 48
    imul rbx, 10
    add rbx, rax ; n <> d = n * 10 + d

    jmp .read_loop
.read_done:
    ret

; input in rax
print_number:
    mov rbp, rsp ; save the stack pointer

    ; TODO: If rax is 0 here, just print "0"

    mov BYTE [rsp-1], 10

.print_loop:
    cmp rax, 0
    je .print_result

    xor rdx, rdx

    mov rcx, 10
    div rcx ; result in rax, remainder in rdx

    ; rax = rest
    ; rdx = digit as a number

    add rdx, 48

    mov BYTE [rsp-1], dl ; push the single lowest byte in rdx
    dec rsp

    jmp .print_loop
.print_result:
    
    mov rax, 1  ; write
    mov rdi, 1  ; stdout
    mov rsi, rsp ; rsp is our buffer pointer

    mov rdx, rbp 
    sub rdx, rsp ; rdx = rbp - rsp = size of our buffer
    syscall

    mov rsp, rbp ; restore the stack
    ret

_start:
    ; initialize the file descriptor
    mov rax, 2
    mov rdi, input_file
    xor rsi, rsi ; flags
    xor rdx, rdx ; mode
    syscall
    mov [fd], rax

.loop:
    inc QWORD [line_index]

.read_until_colon:
    call read_char
    cmp rax, 58
    jne .read_until_colon

.read_group:
    ; we have parsed until the first Game __:
    ;                                       ^
    call read_char ; discard the space

    call read_number ; rbx = count

    call read_char ; rax = first digit of our color

    cmp rax, 114 ; 'r'
    mov rdx, 12
    cmove rcx, rdx ; there are 12 red cubes

    cmp rax, 103 ; 'g'
    mov rdx, 13
    cmove rcx, rdx ; there are 13 green cubes

    cmp rax, 98 ; 'b'
    mov rdx, 14
    cmove rcx, rdx ; there are 14 blue cubes

    ; rcx contains the maximum allowed number of cubes
    cmp rbx, rcx

    jg .invalid_game

.read_until_separator_or_newline:
    call read_char
    cmp rax, 59 ; ';'
    je .read_group
    cmp rax, 44 ; ','
    je .read_group
    cmp rax, 10 ; '\n'
    je .newline_found
    cmp rax, 0 ; EOF
    je .on_eof

    jmp .read_until_separator_or_newline
.newline_found:
    ; this game is valid
    mov rax, [line_index]
    add [total_sum], rax
    jmp .loop


.invalid_game:
    call read_char
    cmp rax, 10 ; '\n'
    je .loop ; we jump to the loop without incrementing the total sum
    cmp rax, 0 ; EOF
    je .on_eof_invalid
    jmp .invalid_game


.on_eof:
    ; this game is valid so we still need to include the last id
    mov rax, [line_index]
    add [total_sum], rax
.on_eof_invalid:

    mov rax, [total_sum]
    call print_number

    ; exit
    mov rax, 60
    mov rdi, 0
    syscall

section .data
    input_buffer: times 4096 db 0
    input_buffer_capacity: dq 0

    fd: dq 0

    input_file: db "input.txt", 0

    line_index: dq 0
    total_sum: dq 0


