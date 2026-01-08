  global main

  section .text

; TODO EXERCISE 3:
; Implement a the tail-recursive factorial function using the unary natural
; number template.
;
; The result should be left in rax and control should be left in fact_done when
; complete.
main:
  ...

; Precondition: ?? Natural ?? Natural
; Postcondition: ?? Natural ??
; Invariant: ?? for some number m
;   (fact n) * acc = (fact^ (m - n) acc) = (fact (n + m))
fact:
  ...

fact_done:
exit:
  mov rax, 60
  syscall

  section .data
dummy:   db 0
