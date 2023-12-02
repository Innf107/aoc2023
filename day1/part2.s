BITS 64
GLOBAL _start

; r15 ~ buffer index

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

read_character_with_lookahead:
    mov rbx, [lookahead_buffer_size]
    cmp [lookahead_index], rbx
    jge .buffer_exceeded

    mov rbx, [lookahead_index]

    mov rcx, [lookahead_buffer]
    mov BYTE al, [rcx+rbx]
    inc QWORD [lookahead_index]
.debug1:
    ret

.buffer_exceeded:
    call read_char
.debug2:
    ret

; argument in rax
save_character:
    mov rbx, [next_lookahead_buffer]
    mov rcx, [next_lookahead_buffer_size]
    mov BYTE [rbx+rcx], al
    inc QWORD [next_lookahead_buffer_size]
    ret

reset_lookahead_buffers:
    ; copy over all remaining elements
    .copy_loop:
        mov rbx, [lookahead_index]
        cmp rbx, [lookahead_buffer_size]
        jge .copy_loop_done
        
        .debug3:
        call read_character_with_lookahead
        call save_character

        jmp .copy_loop
    .copy_loop_done:

    mov QWORD [lookahead_index], 0

    mov rbx, [lookahead_buffer]
    mov rcx, [next_lookahead_buffer]
    mov [lookahead_buffer], rcx
    mov [next_lookahead_buffer], rbx
    
    mov rbx, [next_lookahead_buffer_size]
    mov [lookahead_buffer_size], rbx
    mov QWORD [next_lookahead_buffer_size], 0
    ret

_start:
    ; initialize the file descriptor
    mov rax, 2
    mov rdi, input_file
    xor rsi, rsi ; flags
    xor rdx, rdx ; mode
    syscall
    mov [fd], rax

    mov r14, -1 ; initialize the last digit register

.loop:
    .read_digit_loop:
        call read_character_with_lookahead
    
        cmp rax, 0
        je .done
        cmp rax, 10
        je .loop_done

        cmp rax, 111
        je .one
        cmp rax, 116
        je .t
        cmp rax, 102
        je .f
        cmp rax, 115
        je .s
        cmp rax, 101
        je .eight
        cmp rax, 110
        je .nine

        cmp rax, 48
        jl .otherwise    ; char is smaller than 0 -> not a valid digit

        cmp rax, 57 ; char is larger than 9 -> not a valid digit
        jg .otherwise

        ; rax is an ascii digit
        sub rax, 48
        jmp .read_digit_loop_done

        .otherwise:
            call reset_lookahead_buffers
            jmp .read_digit_loop


        .one:
            call read_character_with_lookahead
            call save_character
            cmp rax, 110
            jne .otherwise
            
            call read_character_with_lookahead
            call save_character
            cmp rax, 101
            jne .otherwise

            mov rax, 1
            jmp .read_digit_loop_done
        .t:
            call read_character_with_lookahead
            call save_character
            cmp rax, 119
            je .two
            cmp rax, 104
            je .three
            jmp .otherwise

            .two:
                call read_character_with_lookahead
                call save_character
                cmp rax, 111
                jne .otherwise

                mov rax, 2
                jmp .read_digit_loop_done
            .three:
                call read_character_with_lookahead
                call save_character
                cmp rax, 114
                jne .otherwise

                call read_character_with_lookahead
                call save_character
                cmp rax, 101
                jne .otherwise
                call read_character_with_lookahead
                call save_character
                cmp rax, 101
                jne .otherwise

                mov rax, 3
                jmp .read_digit_loop_done
        .f:
            call read_character_with_lookahead
            call save_character
            cmp rax, 111
            je .four
            cmp rax, 105
            je .five
            jmp .otherwise

            .four:
                call read_character_with_lookahead
                call save_character
                cmp rax, 117
                jne .otherwise
                call read_character_with_lookahead
                call save_character
                cmp rax, 114
                jne .otherwise

                mov rax, 4
                jmp .read_digit_loop_done
            .five:
                call read_character_with_lookahead
                call save_character
                cmp rax, 118
                jne .otherwise
                call read_character_with_lookahead
                call save_character
                cmp rax, 101
                jne .otherwise

                mov rax, 5
                jmp .read_digit_loop_done
        .s:
            call read_character_with_lookahead
            call save_character
            cmp rax, 105
            je .six
            cmp rax, 101
            je .seven
            jmp .otherwise
            .six:
                call read_character_with_lookahead
                call save_character
                cmp rax, 120
                jne .otherwise

                mov rax, 6
                jmp .read_digit_loop_done
            .seven:
                call read_character_with_lookahead
                call save_character
                cmp rax, 118
                jne .otherwise
                call read_character_with_lookahead
                call save_character
                cmp rax, 101
                jne .otherwise
                call read_character_with_lookahead
                call save_character
                cmp rax, 110
                jne .otherwise

                mov rax, 7
                jmp .read_digit_loop_done
        .eight:
            call read_character_with_lookahead
            call save_character
            cmp rax, 105
            jne .otherwise
            call read_character_with_lookahead
            call save_character
            cmp rax, 103
            jne .otherwise
            call read_character_with_lookahead
            call save_character
            cmp rax, 104
            jne .otherwise
            call read_character_with_lookahead
            call save_character
            cmp rax, 116
            jne .otherwise

            mov rax, 8
            jmp .read_digit_loop_done
        .nine:
            call read_character_with_lookahead
            call save_character
            cmp rax, 105
            jne .otherwise
            call read_character_with_lookahead
            call save_character
            cmp rax, 110
            jne .otherwise
            call read_character_with_lookahead
            call save_character
            cmp rax, 101
            jne .otherwise

            mov rax, 9
            jmp .read_digit_loop_done


        jmp .read_digit_loop
    .read_digit_loop_done:

    call reset_lookahead_buffers

    ; we know rax is a valid digit
    ; r14 := first digit
    ; r12 := last digit

    cmp r14, -1
    cmove r14, rax ; only set the first digit if it was unset

    mov r12, rax ; always override the last digit

    jmp .loop

.loop_done:

    imul r14, 10
    add r14, r12

    mov rax, r14

    ; r14 now contains our two digit number for this line
    add [accumulator], r14 ; add the number in this line to our accumulator

    mov r14, -1 ; reset the first digit

    jmp .loop

.done:
    mov rax, [accumulator]
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

    buffer1: times 10 db 0
    buffer2: times 10 db 0

    lookahead_buffer: dq buffer1
    lookahead_buffer_size: dq 0
    lookahead_index: dq 0

    next_lookahead_buffer: dq buffer2
    next_lookahead_buffer_size: dq 0

    accumulator: dq 0


; one two three four five six seven eight nine
;
; _----+----------+----------+-----------+--------+
; |    |          |          |           |        |
; one  t--+       f----+     s---+       eight    nine
;      |  |       |    |     |   |       
;      wo hree    our  ive   ix  even
