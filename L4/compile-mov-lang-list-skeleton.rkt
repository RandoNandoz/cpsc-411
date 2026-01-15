#lang racket

#|
Consider the following language, mov-lang:

p ::= (begin i ...)
i ::= (set! reg int64)

We want to compile this to an x64 instruction sequence, as a string.
|#

;; EXERCISE: Define 3 example mov-lang programs as quasiquoted lists.
;; mov-lang-p1 must pass the test below.

;; EXAMPLES:
(define mov-lang-p1 '(begin (set! rax 5) (set! rax 10)))
(define mov-lang-p2 '(begin (set! rax 7) (set! rcx 4) (rbx 72093)))
(define mov-lang-p3 '(begin))

(module+ test
 (require rackunit)
 (check-equal? 
  (with-input-from-string "(begin (set! rax 5) (set! rax 10))" read)
  mov-lang-p1))

;; In the functional programming style, we would use the structural
;; recursion design. We pattern match on the data and then process, eventually
;; recurring on the structure of the data.

;; EXERCISE (solo): Implement the compiler.

;; mov-lang -> string?
(define (compile-mov-lang e)
  (define (compile-mov-lang-i i)
    (match i
      [`(set! ,reg ,n)
        (format "mov ~a, ~a" reg n)]))
  (match e
    [`(begin ,i ...) (string-join (map compile-mov-lang-i i) "\n")]))

(module+ test
  (require rackunit)

  (check-equal?
   (compile-mov-lang mov-lang-p1)
   "mov rax, 5\nmov rax, 10"))
