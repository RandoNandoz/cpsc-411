#lang racket

(define rax 0)
(define rcx 0)
(define rdi 0)

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
    (begin
        (set! rdi rcx)
        (exit rdi)))

(define (fact_continue)
    (set! rcx (* rcx rax))
    (set! rax (- rax))
    (fact))

(main)