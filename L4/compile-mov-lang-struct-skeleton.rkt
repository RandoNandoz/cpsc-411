#lang racket

#|
Consider the following language, mov-lang:

p ::= (begin i ...)
i ::= (set! reg int64)

We want to compile this to an x64 instruction sequence, as a string.
|#

;; EXERCISE (together): How *should* we represent mov-lang programs?

;; EXERCISE (together): Define 3 mov-lang programs in this representation

;; EXAMPLES:
(define mov-lang-p1 (void))
(define mov-lang-p2 (void))
(define mov-lang-p3 (void))

;; mov-lang (as list) -> mov-lang (as new representation)
(define (parse-mov-lang p)
 (void))

(module+ test
 (require rackunit)
 (check-equal? 
  (parse-mov-lang '(begin (set! rax 5) (set! rax 10)))
  mov-lang-p1))

;; In the functional programming style, we would use the structural
;; recursion design. We pattern match on the data and then process, eventually
;; recurring on the structure of the data.

;; EXERCISE (solo): Implement the compiler.

;; mov-lang -> string?
(define (compile-mov-lang e)
  (void))

(module+ test
  (require rackunit)

  (check-equal?
   (compile-mov-lang mov-lang-p1)
   "mov rax, 5\nmov rax, 10"))
