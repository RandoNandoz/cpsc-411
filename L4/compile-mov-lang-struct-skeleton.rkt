#lang racket

(provide (all-defined-out))

#|
Consider the following language, mov-lang:

p ::= (begin i ...)
i ::= (set! reg int64)

We want to compile this to an x64 instruction sequence, as a string.
|#

;; EXERCISE (together): How *should* we represent mov-lang programs?

;; begin-term is (structof instrs)
;; 
(struct begin-term (instrs))
(struct set!-term (reg int64))

;; EXERCISE (together): Define 3 mov-lang programs in this representation

;; EXAMPLES:
(define mov-lang-p1 (begin-term (list (set!-term 'rax 5) (set!-term 'rax 10))))
(define mov-lang-p2 (void))
(define mov-lang-p3 (void))

;; mov-lang (as list) -> mov-lang (as new representation)
(define (parse-mov-lang p)
 (define (parse-mov-lang-instr i)
  (match i
    [`(set! ,reg ,n) (set!-term reg n)]))
 (match p
  [`(begin ,i ...)
    (begin-term (map parse-mov-lang-instr i))]))

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
  (define (compile-mov-lang-i i)
    (match i
      [(set!-term reg n)
        (format "mov ~a, ~a" reg n)]))
  (match e
    [(begin-term i) (string-join (map compile-mov-lang-i i) "\n")]))

(module+ test
  (require rackunit)

  (check-equal?
   (compile-mov-lang mov-lang-p1)
   "mov rax, 5\nmov rax, 10"))
