global main

section .text

main:
  mov rax, 5
  mov rcx, 1
  jmp fact

; Precondition: rax is the first argument, and stores a Natural; rcx is the second is also a Natural
; Postcondition: rdi will contain a Natural
; Invariant: for some number m
;   (fact rax) * rcx = (fact^ (m - rax) rcx) = (fact (rax + m))
fact:
  cmp rax, 0
  je fact_done
  imul rcx, rax
  dec rax
  jmp fact

fact_done:
  mov rdi, rcx
exit:
  mov rax, 60
  ;mov rax, 0x200001
  syscall

section .data
dummy:   db 0
