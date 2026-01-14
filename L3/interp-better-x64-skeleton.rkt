#lang racket

(require
  cpsc411/compiler-lib)

(provide
 interp-paren-x64)

#|
Exericse 1 (solo):
Design Parenthsized better-x64 (Paren-x64), which allows users to write
a small subset of x64 statements including moves, multiplication, and
addition over registers, without worrying about boilerplate or interacting
with the operating system.

Describe the grammar, and constraints on the grammar, and any language invariants.

p ::= (s ...)

s ::=
  (mov reg <constant>)
  (mov reg1 reg2)
  (mov reg1 (* reg1 reg2))
  (mov reg1 (+ reg1 reg2))

reg[i] ::= <various x64 registers>
constant ::= any 64 bit number

- invariant: every register must be assigned a value first
- the "value" of the program we can store in rax??

=============
p ::= (begin def ...)
def ::= (define (reg values ...) e)
e ::= (add x x) | (mul x x)

============= william's version

p ::= (begin def ...)
def ::= (define reg value) | e
e ::= (add reg value) | (mul reg value) | (rrmov reg reg)

Constraints:
  - registers must be initialized before they're referenced
  - the final value of the program is the value of rax at the end of the program


Exercise 2 (together): finish the design of Paren-x64
|#

;; Exericse 3 (really just 1.5) (together): Write down the paren-x64-template
(define (paren-x64-template p)
  (define (paren-x64-template-p p)
    (match p
      [`(,s ...) (map parent-x64-template-s s)]))

  (define (parent-x64-template-s s)
    (match s
      [`(mov ,reg ,number)
       ;; prefer this one?
       #:when (number? number)
       (TODO)]
      [`(mov ,reg1 ,reg2)
       #:when (register? reg2)
       (TODO)]
      [`(mov ,reg1 (* ,reg1 ,reg2)) (TODO)]
      [`(mov ,reg1 (+ ,reg1 ,reg2)) (TODO)]))

  (paren-x64-template-p p))

;; any -> void
(define (check-paren-x64-s s)
  ;; re-write data defs - we can do this!
  ;; p ::= (s ...)
  ;; s ::= (mov reg value) | (mov reg1 (binop reg1 reg2))|
  ;; value ::= register | number
  ;; binop ::= + | *
  (match s
    [`(mov ,reg ,number) ;; we can collapse 1 and 2
     #:when (and (register? reg) (number? number))
     (void)]
    [`(mov ,reg1 ,reg2)
     #:when (and (register? reg1) (register? reg2))
     (void)]
    [`(mov ,reg1 (* ,reg1 ,reg2)) ;; and 2 and 3
     #:when (and (register? reg1) (register? reg2))
     (void)]
    [`(mov ,reg1 (+ ,reg1 ,reg2))
     #:when (and (register? reg1) (register? reg2))
     (void)]
    [_ (error "bad paren-x64-s!")]))

;; any -> Paren-x64-p
;; Follow template for output!??!?!?
;; raises an error if the input is invalid
(define (check-paren-x64 p)
  (match p
    [`(,s ...) (for-each check-paren-x64-s s)]
    [_ (error "invalid program!")])
  p)

;; any/c -> boolean?
(define (paren-x64? p)
  (with-handlers ([values (lambda _ #f)])
    (check-paren-x64 p)))

;; Paren-x64-p -> int64
;; Interprets a Paren-x64 program, returning its value.

;; Exericise 5: Finish the design and implement interp-paren-x64
(define/contract (interp-paren-x64 p)
  (-> paren-x64? int64?)

  (define env (make-hasheq))

  (define (interp-paren-x64-p p)
    (match p
      [`(,s ...) (for-each interp-paren-x64-s s)])
    (hash-ref env 'rax))


  (define (interp-paren-x64-value value)
    (match value
      [(? number?)
        value]
      [(? register?)
        (hash-ref env value)]
      [`(,binop ,value1 ,value2)
        ((eval binop)
          (interp-paren-x64-value value1)
          (interp-paren-x64-value value2))]))

  (define (interp-paren-x64-s s)
    (match s
      [`(mov ,reg ,value)
       ;; prefer this one?
       (hash-set! env reg (interp-paren-x64-value value))]))

  (interp-paren-x64-p p))

(module+ test
  (require rackunit)

  (check-equal?
   (interp-paren-x64 '((mov rax 5)))
   5)

  (check-equal?
   (interp-paren-x64 '((mov rax 5) (mov rax (+ rax rax))))
   10)

  (check-equal?
   (interp-paren-x64 '((mov rax 5) (mov rax (+ rax 32))))
   37))
