position 0x8000
text:  db "Hello World.", 0x0A, 0x00

position 0x00
_start:
  mov rdi, text
  call print_string
  hlt
  
print_string:
  load8 [rdi], rax
  cmp rax, 0
  je print_string.null
  jmp print_string.not_null
  
 print_string.null:
  ret
 
 print_string.not_null:
  push16 rdi
  mov rdi, rax
  call put_char
  pop16 rdi
  add rdi, 1
  jmp print_string


put_char:
  push16 rax
  mov rax, rdi
  int 26
  pop16 rax
  ret
  
