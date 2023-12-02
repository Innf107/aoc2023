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

_start:
    ; initialize the file descriptor
    mov rax, 2
    mov rdi, input_file
    xor rsi, rsi ; flags
    xor rdx, rdx ; mode
    syscall
    mov [fd], rax

    mov r14, -1 ; initialize the last digit register

; r13 := total sum we have accumulated so far

.loop:
    call read_char
    cmp rax, 0 ; check for EOF
    je .done

    cmp rax, 10 ; check for newlines
    je .loop_done

    sub rax, 48 ; convert digit to number

    cmp rax, 0
    jl .loop    ; char is smaller than 0 -> not a valid digit

    cmp rax, 9 ; char is larger than 9 -> not a valid digit
    jg .loop


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

    ; r14 now contains our two digit number for this line
    add r13, r14 ; add the number in this line to our accumulator

    mov r14, -1 ; reset the first digit

    jmp .loop

.done:
    mov rax, r13
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


