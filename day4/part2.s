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

    ; r13 := number of won scratch cards

    ; We need to
    ; - add current_count to each of the first r13 values in next_scratch_card_counts
    ; - shift over by one and update current_count filling up with a 1
    ; but we can do both in a single, fused loop


    mov rbx, [current_count]

    ; calculate the next current_count

    ; rcx = if current_count > 0 then next_scratch_card_counts[0] + current_count else next_scratch_card_counts[0]
    mov rcx, [next_scratch_card_counts]
    mov rdx, rcx
    add rdx, [current_count]
    cmp r13, 0
    cmovg rcx, rdx

    mov rbx, [current_count] ; rbx is the current_count of this iteration
    mov [current_count], rcx ; we already move over the next current_count

    add [total_sum], rbx

    mov rax, 0
    dec r13
    .shift_add_loop:
        cmp rax, 9
        jge .shift_add_loop_done

        mov rcx, [next_scratch_card_counts+rax*8+8]
        
        ; only increment our moved over count if its index is less than r13
        mov rdx, rcx
        add rdx, rbx

        cmp rax, r13
        cmovl rcx, rdx

        mov [next_scratch_card_counts+rax*8], rcx

        inc rax
        jmp .shift_add_loop
    .shift_add_loop_done:
    ; fill the last number up with 1
    mov QWORD [next_scratch_card_counts+9*8], 1

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

    current_count: dq 1
    next_scratch_card_counts: times 10 dq 1

; total: 2, current: 2
; Card 1: 2 winning
; Card 2: 3 winning
;>Card 3: 1 winning
;
; next: [3, 1, 1, 1, 1, 1, 1, 1, 1]


