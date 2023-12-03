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
    mov rdx, 4096
    syscall

    cmp rax, 0
    je .on_eof

    mov [input_buffer_capacity], rax

    mov r15, 0 ; reset the buffer index

.skip_refill:
    xor rax, rax
    mov BYTE al, [input_buffer + r15]
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

on_symbol:
    ; remember that we hit a symbol for numbers on the next line
    mov BYTE [current_symbols+r14], 1

    mov rcx, [previous_numbers]

    ; check for numbers on the previous line
    xor rax, rax ; rax is our loop counter
.previous_number_loop:
    cmp rax, [previous_numbers_offset]
    jge .previous_number_loop_done

    cmp r14, [rcx+rax]   ; start of our range (already offset by one to account for the diagonals)
    jl .skip_number
    cmp r14, [rcx+rax+8] ; rcx := end of our range (already offset by one to account for the diagonals)
    jg .skip_number

    ; this number is valid
    mov rdx, [rcx+rax+16]
    add [total_sum], rdx
    ; reset its bounds so we don't count it twice
    mov QWORD [rcx+rax], 0
    mov QWORD [rcx+rax+8], 0

.skip_number:
    add rax, 3*8
    jmp .previous_number_loop
.previous_number_loop_done:
    mov r13, 1
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
    ; increment r14 immediately so that our indices are 1-based and we can one before the first character in a line
    inc r14
    ; r14 := column index
    call read_char
    
    cmp rax, 0 ; eof
    je .on_eof
    cmp rax, 10 ; newline
    je .on_newline
    cmp rax, '.'
    je .on_dot

    cmp rax, '0'
    jl .symbol
    cmp rax, '9'
    jg .symbol

    ; rax is a digit
    sub rax, '0' ; convert digit to its value

.start_reading_number:
    mov r12, rax ; r12 := the number we are parsing

    ; remember the current position - 1 (we need to check diagonals as well)
    mov [initial_number_position], r14
    dec QWORD [initial_number_position]

.read_number_loop:
    mov rbx, 1
    ; check if any of the adjacent characters on the previous line are symbols
    cmp BYTE [previous_symbols+r14-1], 1
    cmove r13, rbx
    cmp BYTE [previous_symbols+r14], 1
    cmove r13, rbx
    cmp BYTE [previous_symbols+r14+1], 1
    cmove r13, rbx

    inc r14
    call read_char

    cmp rax, 10 ; newline
    je .on_number_newline
    cmp rax, '.'
    je .number_non_symbol

    cmp rax, '0'
    jl .number_symbol
    cmp rax, '9'
    jg .number_symbol

    sub rax, '0'

    ; if we're here, we have read one additional digit
    imul r12, 10
    add r12, rax ; append digit in rax to our parsed number

    jmp .read_number_loop

.on_number_newline:
    call .number_shared
    jmp .on_newline

.number_symbol:
    mov r13, 1
    call .number_shared
    call on_symbol
    jmp .loop

.number_non_symbol:
    call .number_shared
    xor r13, r13
    jmp .loop

; should be called
.number_shared:
    ; if this one was a valid part number we treat it as if we hit a symbol
    cmp r13, 1
    je .is_already_part_number

    mov rcx, [current_numbers]

    ; we save the number position if it is not necessarily a valid part number
    mov rbx, [current_numbers_offset]

    mov rdx, [initial_number_position]
    mov QWORD [rcx+rbx], rdx
    ; we are already one character past the number so we don't need to increment here to get the diagonals
    mov [rcx+rbx+8], r14
    mov [rcx+rbx+16], r12 ; save the number itself
    add QWORD [current_numbers_offset], 3*8
    ret

.is_already_part_number:
    add [total_sum], r12
    ret


.symbol:
    ; remember the symbol for the next line
    mov BYTE [current_symbols+r14], 1
    mov r13, 1
    call on_symbol
    jmp .loop

.on_dot:
    xor r13, r13
    jmp .loop

.on_newline:
    xor r13, r13 ; reset our knowledge about the next number being valid

    ; swap over our buffers
    xor rax, rax
.carry_symbols_over_loop:
    cmp rax, 144
    jge .carry_done

    mov QWORD rbx, [current_symbols+rax]
    mov QWORD [previous_symbols+rax], rbx
    mov QWORD [current_symbols+rax], 0 

    add rax, 8
    jmp .carry_symbols_over_loop
.carry_done:

    ; swap the number buffers and reset the current size
    mov rcx, [current_numbers]
    mov rdx, [previous_numbers]
    mov [previous_numbers], rcx
    mov [current_numbers], rdx

    ; carry over the current size to the previous
    mov rcx, [current_numbers_offset]
    mov [previous_numbers_offset], rcx
    ; reset the current size
    mov QWORD [current_numbers_offset], 0

    xor r14, r14 ; reset our column index
    jmp .loop

.on_eof:
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

    previous_symbols: times 144 db 0
    current_symbols: times 144 db 0


    ; there are only going to be 141 characters but we might read ahead
    ; so let's use 200 for good measure
    buffer1: times 3*200 dq 0
    buffer2: times 3*200 dq 0

    previous_numbers: dq buffer1
    previous_numbers_offset: dq 0
    ; current_numbers : array<(start, end, number)>*
    current_numbers: dq buffer2
    current_numbers_offset: dq 0

    initial_number_position: dq 0

    total_sum: dq 0
