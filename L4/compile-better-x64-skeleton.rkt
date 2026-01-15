#lang racket

#|
better-x64 is a new programming language with the same concrete syntax as x64, but does not require boilerplate, and is linked against a run-time system.
It includes only the instructions of x64, represented as strings.
|#

;; Exercise 4: implement fact in better-x64
(define fact_s
#<<EOS
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
EOS
)

;; Exercise 5: Design and implement compiler-better-x64, which takes a better-x64 program `p`, and generated a valid x64 program that can be compiled with `nasm` and executed.
(define (compile-better-x64 p)
  (~a #:separator "\n"
    "global start"
    "section .text"
    "start"
    p
    "exit:"
    "mov rax, 60"
    "syscall"
    "section .data"
    "s: db 0"))

(module+ test
 (require rackunit)
 (require cpsc411/compiler-lib)
 
 (current-pass-list (list values))
 (check-equal?
  (execute (compile-better-x64 "mov rdi, 120") nasm-run/exit-code)
  120))
