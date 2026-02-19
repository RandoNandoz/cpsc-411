#lang racket

(module+ test
  (require rackunit))

#|
A language:
1. Mutable variables
2. Structured control flow
3. Operations on integers

p      ::= tail
pred   ::= (relop triv triv)
           (true)
           (false)
           (not pred)
           (begin effect ... pred)
           (if pred pred pred)
tail   ::= value
           (begin effect ... tail)
           (if pred tail tail)
value  ::= triv
           (binop triv triv)
           (if pred value value)
           (begin effect ... value)
effect ::= (set! aloc value)
           (if pred effect effect)
           (begin effect ... effect)
triv   ::= integer? | aloc
relop  ::= < | <= | = | >= | > | !=
binop  ::= + | * | -
|#

(define eg1
  `(if (true) x.1 x.2))

(define eg2
  `(if (<= 1 2) 5 6))

(define eg3
  `(if (begin (set! x.1 1) (<= x.1 2)) 5 6))

(define eg-max `(if (< x.1 y.1) y.1 x.1))

(define (runtime d p)
  `(let ([true (lambda () #t)]
         [false (lambda () #f)])
     ,(for/fold ([r p])
                ([(k v) (in-dict d)])
        `(let ([,k ,v]) ,r))))

(define (interp-lang d p)
  (eval (runtime d p) (module->namespace 'racket/base)))

(module+ test
  (check-equal?
   (interp-lang '((x.1 . 2) (x.2 . 3))
                eg1)
   2)

  (check-equal?
   (interp-lang '((x.1 . 2) (x.2 . 3))
                eg2)
   5)

  (check-equal?
   (interp-lang '() eg2)
   5)

  (check-equal?
   (interp-lang '((x.1 . 0))
                eg3)
   5)

  (check-equal?
   (interp-lang '((x.1 . 2) (y.1 . 3))
                eg-max)
   3)

  (check-equal?
   (interp-lang '((x.1 . 3) (y.1 . 1))
                eg-max)
   3))

;; I want to implement an optimization to evaluate away some predicates.
;;
;; E.g.
;; `(if (true) x.1 x.2) -> x.1
;; but
;; `(if (<= x.1 x.2) 5 6) -> `(if (<= x.1 x.2) 5 6)
;;


;; TODO: Finish designing and implementing abstract-interp
;;
;; Carefully consider the signatures, any additional arguments, purpose, and
;; invariants on accumulators (if any).

(define (abstract-interp p)
  (define (abstract-interp-tail tail)
    (void))

  (define (abstract-interp-pred pred)
    (void))

  (define (abstract-interp-effect effect)
    (void))

  (define (abstract-interp-value value)
    (void))

  (define (abstract-interp-triv triv)
    (void))

  (define (abstract-interp-relop relop)
    (void))

  (define (abstract-interp-binop binop)
    (void))

  (abstract-interp-tail p))

(module+ test
  ;; NOTE: Fragile syntactic tests; really should be behavioural property based
  ;; tests, but the syntactic tests help us ensure the optimization is working
  ;; as desired.
  ;; Failures of these tests may be false positives if the results compute the
  ;; same.

  (check-equal?
   (abstract-interp 'x.1)
   'x.1)

  (check-equal?
   (abstract-interp `(if (true) x.1 x.2))
   'x.1)

  (check-equal?
   (abstract-interp `(if (<= x.1 x.2) 5 6))
   `(if (<= x.1 x.2) 5 6)))
