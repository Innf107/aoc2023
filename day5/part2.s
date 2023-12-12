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
        mov [inputs+r13*8], rbx ; save the start position
        mov rcx, rbx
        call read_number
        add rbx, rcx    ; transform from length to end position
        mov [inputs+r13*8+8], rbx

        add r13, 2
        cmp rax, 10 ; '\n'
        je .read_initial_seed_loop_done

        jmp .read_initial_seed_loop
    .read_initial_seed_loop_done:
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

            ; check for case 1 (unrelated ranges)
            cmp [inputs+rbx*8+8], r13 ; compare with the start of our range (inclusive)
            jle .skip_seed
            cmp [inputs+rbx*8], r14 ; compare with the end of our range (exclusive)
            jge .skip_seed

            cmp [inputs+rbx*8], r13
            jge .left_contained

            ; right contained

            cmp [inputs+rbx*8+8], r14
            jle .right_overlap

            ; case 5: double overlap
            mov rcx, [inputs+rbx*8]     ; rcx = previous start
            mov rsi, [inputs+rbx*8+8]   ; rsi = previous end

            mov [inputs+rbx*8], r13     ; we restrict the mapped range to the entire range of the mapping
            mov [inputs+rbx*8+8], r14   ; ^

            add [inputs+rbx*8], r12     ; actually apply the mapping to both endpoints
            add [inputs+rbx*8+8], r12   ; ^

            ; create both split off ranges
            ; left split off range
            mov rdx, [input_count]
            mov [inputs+rdx*8], rcx     ; our new range starts where the old one would have started
            mov [inputs+rdx*8+8], r13   ; our new range ends where we split off the old one

            add QWORD [input_count], 2 ; remember that we created a new range

            ; right split off range
            mov rdx, [input_count]    ; index of our new range
            mov [inputs+rdx*8], r14   ; our new range starts where we restriced the old one
            mov [inputs+rdx*8+8], rsi ; and ends where the old one would have ended

            add QWORD [input_count], 2 ; remember that we created a new range
            jmp .skip_seed

            ; case 4
            .right_overlap:
            mov rcx, [inputs+rbx*8]    ; rcx = previous start
            mov [inputs+rbx*8], r13    ; restrict the left endpoint

            add [inputs+rbx*8], r12   ; apply our mapping to the left (now modified) endpoint
            add [inputs+rbx*8+8], r12 ; apply our mapping to the right endpoint

            mov BYTE [modified+rbx],  1 ; mark the old (now restricted) range as modified

            ; create the split off range
            mov rdx, [input_count]
            mov [inputs+rdx*8], rcx     ; our new range starts where the old one would have started
            mov [inputs+rdx*8+8], r13   ; our new range ends where we split off the old one

            add QWORD [input_count], 2 ; remember that we created a new range

            ; we DON'T mark it as modified since it might be mapped over separately
            jmp .skip_seed


            ; either case 2 or 3
            .left_contained:
            cmp [inputs+rbx*8+8], r14
            jle .entirely_contained

            ; case 3: left overlap

            mov rcx, [inputs+rbx*8+8] ; rcx = previous end
            mov [inputs+rbx*8+8], r14 ; restrict the right endpoint

            add [inputs+rbx*8], r12   ; apply our mapping to the left endpoint
            add [inputs+rbx*8+8], r12 ; apply our mapping to the right (now modified) endpoint
            
            mov BYTE [modified+rbx],  1 ; mark the old (now restricted) range as modified

            ; create the split off range
            mov rdx, [input_count]  ; index of our new range
            mov [inputs+rdx*8], r14   ; our new range starts where we restriced the old one
            mov [inputs+rdx*8+8], rcx ; and ends where the old one would have ended

            add QWORD [input_count], 2 ; remember that we created a new range

            ; we DON'T mark it as modified since it might be mapped over separately
            jmp .skip_seed

            ; case 2
            .entirely_contained:
            add [inputs+rbx*8], r12     ; apply our offset to both endpoints
            add [inputs+rbx*8+8], r12   ; ^ 
            mov BYTE [modified+rbx], 1 ; mark as modified

            .skip_seed:

            add rbx, 2
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

        add rax, 2
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

    inputs: times 8000 dq 0
    input_count: dq 0

    modified: times 8000 db 0

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


; inputs: [10 20]
; 
; ; case 1 (out of range)
; 
; ----
;           -----
; 
; start end offset
; 30    50  100  --> [10 20]
; 
; 
;
; ; case 2 (contained in the range)
;         ----
;     ------------
; 
; 0     30  100  --> [110 120]
; 
; ; case 3 (left overlap)
;     ----|----
; --------
; 
; 0     15  100  -->  [110 114] [15 20]
; 
; ; case 4 (right overlap)
;     ----|----
;         -----------
; 17    34 100  --> [10, 16] [117 120]
; 
; case 5 (double overlap) 
;      -----|----|-------
;            ----
;   15 17 100 --> [10 14] [115 116] [17 20]
;