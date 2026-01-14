#lang racket

;; Let us translate our x64 program back, into Racket.
;; But this time, let us try to faithfully emulate x64 features in Racket.

(define rax (void))
(define rcx (void))
(define rdi (void))

(define (main)
  (set! rax 5)
  (set! rcx 1)
  (fact))

(define (fact)
  (begin
    (if (zero? rax)
        (fact_done)
        (fact_continue))
    (void)))

(define (fact_done)
  (set! rdi rcx)
  (exit rdi))

(define (fact_continue)
  (set! rcx (* rcx rax))
  (set! rax (- rax 1))
  (fact))

(main)
