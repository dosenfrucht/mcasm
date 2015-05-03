position 0

__start:
  call is_zero
  -- result in rax
  
position 0xFF00 -- higher half - just for layout testing

is_zero:
  cmp rdi, 0
  je is_zero.if0_true
  jmp is_zero.if0_false
  
is_zero.if0_true:
  mov rax, 1
  jmp is_zero.if0_cont
  
is_zero.if0_false:
  mov rax, 0
  jmp is_zero.if0_cont
  
is_zero.if0_cont:
  ret
