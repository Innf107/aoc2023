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

    ; parse our initial inputs
    .initial_skip_until_colon_loop:
        call read_char
        cmp rax, ':'
        jne .initial_skip_until_colon_loop

    mov r13, 0 ; r13 := index into our input array
    ; read initial inputs (seeds)
    .read_initial_seed_loop:
        call read_number
        mov [inputs+r13*8], rbx ; save the read number

        cmp rax, 10 ; '\n'
        je .read_initial_seed_loop_done

        inc r13
        jmp .read_initial_seed_loop
    .read_initial_seed_loop_done:
    inc r13
    mov [input_count], r13

.loop:
    nop
    ; skip until first ':'
    .skip_until_map_colon_loop:
        call read_char
        cmp rax, ':'
        jne .skip_until_map_colon_loop
    call read_char ; skip the newline

    .process_entry_loop:
        ; process each entry in the current map
        call read_number
        mov r12, rbx ; r12 := destination
        
        call read_number
        mov r13, rbx ; r13 := source

        call read_number
        mov r14, rbx ; r14 := length

        ; convert to an easier format
        add r14, r13 ; r14 := end
                    ; r13 := start
        sub r12, r13 ; r12 := offset

        ; process each input
        xor rbx, rbx
        .process_input_loop:
            cmp rbx, [input_count]
            jge .process_input_loop_done

            ; NOT *8 because modified is a byte arary
            cmp BYTE [modified+rbx], 1
            je .skip_seed

            cmp [inputs+rbx*8], r13 ; compare with the start of our range (inclusive)
            jl .skip_seed
            cmp [inputs+rbx*8], r14 ; compare with the end of our range (exclusive)
            jge .skip_seed

            ; input is in our range
            add [inputs+rbx*8], r12 ; apply our offset mapping
            mov BYTE [modified+rbx], 1

            .skip_seed:

            inc rbx
            jmp .process_input_loop
        .process_input_loop_done:

        ; read one character ahead to see if we hit the end of one map
        call read_char
        cmp rax, 0 ; EOF
        je .on_eof
        cmp rax, 10 ; '\n' we hit the end of our loop
        je .process_entry_loop_done

        ; if we haven't jumped out, we need to restore this lookahead character
        mov BYTE [lookahead_char], al

        jmp .process_entry_loop
    .process_entry_loop_done:

    ; reset our modified buffer
    mov rax, 0
    .reset_loop:
        cmp rax, [input_count]
        jge .reset_done

        ; NOT *8 since modified is a byte array
        mov BYTE [modified+rax], 0

        inc rax
        jmp .reset_loop 
    .reset_done:


    jmp .loop

.on_eof:
    ; find the smallest number in our "inputs" (which are now the fully mapped locations)
    mov rbx, [inputs] ; rbx contains our maximum input
    mov rax, 0
    .find_min_loop:
        cmp rax, [input_count]
        jge .find_min_loop_done

        cmp [inputs+rax*8], rbx
        cmovl rbx, [inputs+rax*8]

        inc rax
        jmp .find_min_loop
    .find_min_loop_done:

    mov rax, rbx
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

    inputs: times 100 dq 0
    input_count: dq 0

    modified: times 100 db 0


;

; we have 7 maps
;
; the input format
; destination    source    length
; X              Y         Z
;
; the format we actually want
; source   end   offset
; Y        Y+Z   X-Y

