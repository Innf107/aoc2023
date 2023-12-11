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
; allows leading spaces
read_number:
    mov rbx, 0

.skip_leading_spaces_loop:
    call read_char
    cmp rax, ' '
    je .skip_leading_spaces_loop

.read_loop:
    cmp rax, '0'
    jl .read_done
    cmp rax, '9'
    jg .read_done

    sub rax, '0'
    imul rbx, 10
    add rbx, rax ; n <> d = n * 10 + d

    call read_char
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
    ; Battle plan:
    ; - skip all characters until the first ':' (unless there is an EOF)
    ; - read 10 numbers into winning_numbers
    ; - count the number of remaining numbers present in winning_numbers
    ; - add 2^winning_numbers to our total sum

    ; skip all characters until the first ':' (unless there is an EOF)
    .skip_card_number_loop:
        call read_char
        cmp rax, 0 ; EOF
        je .on_eof

        cmp rax, ':'
        je .skip_card_number_loop_done

        jmp .skip_card_number_loop
    .skip_card_number_loop_done:
    
    ; skip the space after the first ':'
    call read_char

    mov r14, 0
    .read_winning_numbers_loop:
        cmp r14, 10
        jge .read_winning_numbers_loop_done

        call read_number
        mov [winning_numbers+8*r14], rbx

        inc r14
        jmp .read_winning_numbers_loop
    .read_winning_numbers_loop_done:

    ; skip the "|" character
    call read_char

    mov r13, 0 ; matching number count

    .compare_own_numbers_loop:
        call read_number
        
        mov rcx, 0
        .check_in_winning_numbers_loop:
            cmp rcx, 10
            jge .check_in_winning_numbers_loop_done

            cmp rbx, [winning_numbers+8*rcx]
            je .found_winning_number
            inc rcx
            jmp .check_in_winning_numbers_loop

            .found_winning_number:
                inc r13
                jmp .check_in_winning_numbers_loop_done
        .check_in_winning_numbers_loop_done:

        cmp rax, 10 ; '\n'
        je .compare_own_numbers_loop_done

        jmp .compare_own_numbers_loop
    .compare_own_numbers_loop_done:
    cmp r13, 0 ; no numbers matched, so this card is worth 0 points and we can skip it
    je .loop

    dec r13

    mov rax, 1

    ; idk how to do a bitshift in nasm so i guess we won't do that
    .power_of_2_loop:
        cmp r13, 0
        jle .power_of_2_loop_done

        imul rax, 2
        dec r13
        jne .power_of_2_loop
    .power_of_2_loop_done:

    add [total_sum], rax
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

    lookahead_char: db 0

    total_sum: dq 0

    winning_numbers: times 10 dq 0


