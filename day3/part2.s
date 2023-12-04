BITS 64
GLOBAL _start


; r15 ~ buffer index

; does not mutate rbx
read_char:
    cmp BYTE [lookahead_char], 0
    jne .use_lookahead

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
.use_lookahead:
    xor rax, rax
    mov BYTE al, [lookahead_char]
    mov BYTE [lookahead_char], 0
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


; takes a pointer to the gear in r9 and a number in rdx
; sets rax to
try_add_number_to_gear:
    cmp QWORD [r9+8], 0
    jne .first_already_set
    ; this is the first number
    mov [r9+8], rdx
    ret

    .first_already_set:
    ; this is either the second number or an invalidating number
    cmp QWORD [r9+16], 0
    jne .invalidate

    ; this is the second number
    mov [r9+16], rdx
    ; so we need to add its gear ratio to the total sum
    imul rdx, [r9+8]
    add [total_sum], rdx
    ret

    .invalidate:
    ; TODO: We *might* need to check that we don't invalidate gears twice?

    ; we found a "gear" that actually has 3 adjacent part numbers so we invalidate it
    ; and remove its incorrect gear ratio from the total sum
    mov QWORD [r9], -1
    imul rdx, [r9+8]
    sub [total_sum], rdx
    mov rax, -1
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
    inc r14

    call read_char

    cmp rax, 0 ; EOF
    je .on_eof

    cmp rax, 10 ; \n
    je .on_newline

    cmp rax, '*'
    je .on_gear

    cmp rax, '0'
    jl .on_ignored

    cmp rax, '9'
    jg .on_ignored

    .on_number:
    ; rax is a digit
    sub rax, '0'

    ; rax is a number so we need to do the following
    ; - remember the first digit's position                             DONE
    ; - parse the remaining number                                      DONE
    ; - remember this number for the next line                          DONE
    ; - remember this number for a (potential) gear immediately after   DONE
    ; - check for immediately preceding gears                           DONE
    ; - check for diagonally gears on the previous line                 DONE
    ; - for every passed position do                                    DONE
    ;    - check the previous line                                      DONE
    
    ; remember the first digit's position
    mov r13, r14 ; r13 := initial digit's position
    dec r13 ; decrement r13 by one since we want to include the diagonals

    ; parse the remaining number
    mov r12, rax
    .parse_number_loop:
    inc r14
    call read_char
    cmp rax, '0'
    jl .parse_number_done
    cmp rax, '9'
    jg .parse_number_done

    ; rax is a digit
    sub rax, '0'
    imul r12, 10
    add r12, rax

    jmp .parse_number_loop
    .parse_number_done:

    ; reset our lookahead character so that loop processes the first non-digit character
    mov [lookahead_char], rax

    ; r12 := our parsed number

    ; remember this number for the next line
    mov r9, [current_numbers]
    add r9, [current_numbers_offset]
    mov [r9], r13
    mov [r9+8], r14
    mov [r9+16], r12
    add QWORD [current_numbers_offset], 3*8

    mov [preceding_number], r12

    cmp QWORD [preceding_gear], 0
    je .skip_preceding_gear

    ; there is a preceeding gear
    mov r9, [preceding_gear]
    mov rdx, r12
    call try_add_number_to_gear

    .skip_preceding_gear:


    ; check for gears on the previous line including diagonals.
    ; r14 is already one past the last digit so we need to go from r13 to r14 inclusive

    .previous_gear_loop:    
    cmp r13, r14
    jg .previous_gear_loop_done

    mov r10, [previous_gears]; r10 := boundary
    add r10, [previous_gears_offset]

    mov r9, [previous_gears]
    .check_previous_gear_loop:
        cmp r9, r10
        jge .check_previous_gear_loop_done

        cmp [r9], r13
        jne .skip_try_add_number
        mov rdx, r12
        call try_add_number_to_gear
        .skip_try_add_number:

        add r9, 3*8
        jmp .check_previous_gear_loop
    .check_previous_gear_loop_done:

    inc r13
    jmp .previous_gear_loop
    .previous_gear_loop_done:
    
    ; decrement since we added the first non-digit to our lookahead thingy
    dec r14
    jmp .loop

.on_ignored:
    mov QWORD [preceding_gear], 0
    mov QWORD [preceding_number], 0
    jmp .loop

.on_gear:
    ; If we find a gear we need to
    ; - keep track of this gear for the next line
    ; - check if there is an immediately preceding number
    ; - check the previous line for numbers
    ; - keep track of this gear for a (potential) number immediately after

    ; keep track of this gear for the next line
    mov r9, [current_gears]
    add r9, [current_gears_offset]
    mov [r9], r14        ; save the position
    mov QWORD [r9+8], 0  ; zero out the part numbers
    mov QWORD [r9+16], 0 ; ^

    add QWORD [current_gears_offset], 3*8

    ; check if there is an immediately preceding number
    cmp QWORD [preceding_number], 0
    je .skip_preceding_number

    ; there was a number immediately before this one
    mov rax, [preceding_number]
    mov [r9+8], rax

    .skip_preceding_number:

    ; check the previous line for numbers
    xor rax, rax
    .previous_number_loop:
        cmp rax, [previous_numbers_offset]
        jge .previous_number_loop_done

        mov rbx, [previous_numbers]
        mov rcx, [rbx+rax]   ; rcx := start position
        cmp r14, rcx
        jl .skip_number

        mov rcx, [rbx+rax+8] ; rcx := end position
        cmp r14, rcx
        jg .skip_number

        ; we have hit this number
        mov rdx, [rbx+rax+16] ; rcx := number value
        call try_add_number_to_gear
        cmp rax, -1
        je .previous_number_loop_done

        .skip_number:
        add rax, 3*8
        jmp .previous_number_loop
    .previous_number_loop_done:

    ; keep track of this gear for numbers immediately after
    mov [preceding_gear], r9

    ; reset the preceding_number_flag
    mov QWORD [preceding_number], 0
    jmp .loop

.on_newline:
    xor r14, r14
    ; swap our gear buffers
    mov rax, [previous_gears]
    mov rbx, [current_gears]
    mov [previous_gears], rbx
    mov [current_gears], rax
    ; carry over our gear buffer offset
    mov rax, [current_gears_offset]
    mov [previous_gears_offset], rax
    mov QWORD [current_gears_offset], 0

    ; swap our number buffers
    mov rax, [previous_numbers]
    mov rbx, [current_numbers]
    mov [previous_numbers], rbx
    mov [current_numbers], rax
    ; carry over our number buffer offset
    mov rax, [current_numbers_offset]
    mov [previous_numbers_offset], rax
    mov QWORD [current_numbers_offset], 0


    ; clear preceeding
    mov QWORD [preceding_gear], 0
    mov QWORD [preceding_number], 0
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

    ; gear_buffer1 : array<(position, number1, number2)>
    gear_buffer1: times 3*200 dq 0
    gear_buffer2: times 3*200 dq 0

    previous_gears: dq gear_buffer1
    previous_gears_offset: dq 0

    current_gears:  dq gear_buffer2
    current_gears_offset: dq 0

    ; there are only going to be 141 characters but we might read ahead
    ; so let's use 200 for good measure
    number_buffer1: times 3*200 dq 0
    number_buffer2: times 3*200 dq 0

    previous_numbers: dq number_buffer1
    previous_numbers_offset: dq 0
    ; current_numbers : array<(start, end, number)>*
    current_numbers: dq number_buffer2
    current_numbers_offset: dq 0

    preceding_number: dq 0

    ; 0 or a pointer to the preceeding gear in current_gears
    preceding_gear: dq 0

    total_sum: dq 0

    lookahead_char: db 0

