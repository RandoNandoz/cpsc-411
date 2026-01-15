#lang racket

(require
 racket/class)

#|
Consider the following language:

p ::= (begin i ...)
i ::= (set! reg int64)

In a typical OO style, we might use a different representation.
We might implement the compiler for this using the visitor pattern, as below.
We develop a class for each node in the abstract syntax tree.
Each class has it's own "compile" method, which implements the visitor pattern.
|#

(define term-class%
  (class object%
    (super-new)
    (inspect #f) ; so equal? works
    (define/public (compile)
      (error "Undefined method"))))

;; First, a begin class for representing `(begin i ...)
(define begin-class%
  (class term-class%
    (super-new)
    (inspect #f) ; so equal? works
    (init-field instruction-ls)

    ;; () -> String
    (define/override (compile)
      (void))))

;; Then a set! class for representing `(set! reg integer)
(define set!-class%
  (class term-class%
    (super-new)
    (inspect #f) ; so equal? works
    (init-field opand1)
    (init-field opand2)

    ;; () -> String
    (define/override (compile)
      (void))))

;; EXERCISE: Define 3 mov-lang programs using this representation.

;; EXAMPLES:

(define mov-lang-p1 (void))

(define mov-lang-p2 
 (new begin-class%
  [instruction-ls '()]))

(define mov-lang-p3 (void))

;; Then we need a reader/parser, to transform the data representation into the
;; correct class structure.

;; mov-lang (as list) -> term-class%
(define (parse-oo e)
  (void))

(module+ test
 (require rackunit)
 (check-equal? 
  (parse-oo `(begin (set! rax 5) (set! rax 10))) 
  mov-lang-p1))

;; Finally, we can compile by "parsing" then calling the compile method.

;; mov-lang -> string?
(define (compile-oo e)
  (send (parse-oo e) compile))

;; There are trade offs.
;;
;; The OO style would save us from recompiling the entire code base after adding
;; a new AST node.
;; We could add a new class, with a new visit method, and only compile that new
;; class.
;; In the FP style, we would need to modify the pattern match clause, and
;; recompile the affected module.
;;
;; However, in the FP style, we work directly over the data representation,
;; which frees us from writing a parser from data to classes.
;; It also allows us to handle the data a little more generically, by writing
;; code that is abstract with respect to parts of the data, rather than
;; transforming all of the data into concrete classes.

(module+ test
  (check-equal?
   (compile-oo `(begin (set! rax 5) (set! rax 10)))
   "mov rax, 5\nmov rax, 10"))
