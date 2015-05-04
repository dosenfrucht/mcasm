-- data at 0x0010
position 0x0010
str:
    db "Hello, world."
times 3 db 0


-- code entry
position 0x0
boot:
    mov rdi, str
    mov rsi, 13
    call print_string
    hlt

-- functions
position 0x8000


-- print_string(char* c, int i)
--   if i == 0
--    then ()
--    else print_char(*c) >> print_string(c + 1, i - 1)
print_string:
    cmp rsi, 0
    je  print_string.if0_false
    jmp print_string.if0_true
  
  print_string.if0_false:
    ret
 
  print_string.if0_true:
    mov r8, rdi
    load8 [rdi], rdi
    call print_char
    mov rdi, r8
    sub rsi, 1
    add rdi, 1
    jmp print_string

print_char:
 -- add logic for printing a character
