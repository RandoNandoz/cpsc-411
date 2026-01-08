#lang racket

(module+ test
  (require rackunit))

;; TODO EXERCISE 1:
;; Implement the factorial function, recursively, using the unary natural number
;; template.

;; Natural -> Natural
;; Returns the factorial of n.
(define (fact n)
  (void))

(module+ test
  (check-equal? (fact 5) 120))

;; TODO EXERCISE 1:
;; Implement the factorial function, tail-recursively, using the unary natural number
;; template with an accumulator

;; Natural Natural -> Natural
;; Returns the factorial of n using constant stack space.
;;
;; INVARIANT: acc is an accumulator such that for some number m, such
;; that (fact n) * acc = (fact^ (m - n) acc) = (fact (n + m))
(define (fact^ n acc)
  (void))

(module+ test
  (check-equal?
   (fact^ 5 1)
   120)

  (check-equal?
   (fact^ 5 1)
   (fact 5))

  (check-equal?
   (* (fact 5) 1)
   (fact^ 5 1))

  (check-equal?
   (* (fact 4) 5)
   (fact^ 4 5))

  (check-equal?
   (* (fact 4) 5)
   (fact 5)))
