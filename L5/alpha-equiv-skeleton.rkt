#lang racket

#|
e ::= x | number | (let ([x e] ...) e)
|#

#|
I want to *decide* when are two `e`s equivalent.

e e -> boolean?

"Decide" -> always return #t of #f for all possible inputs.

Soundly approximate (w.r.t. contextual equivalence)
program equivalence on `e`s, using alpha-equivalence.

"Sound" -> my approximation will return #t when it's "actually" #t
        -> may return "#f" when it's actually "#t"
        -> will never return "#t" when it's actually "#f"

"Complete" -> if the property actually "#t", then my approximation
              certainly returns "#t"


Specifically, two expressions are alpha-equivalent if they only differ in bound names, but are otherwise equal. 
That is, two expressions are equal up to bound names.

|#

(define (let?))

(define (rename-let e1 e2)
  (for/fold   ))

(define (alpha-equivalent? input-e1 input-e2)
(define (alpha-equivalent? e1 e2 context)
  (match* (e1 e2)
    [`(,n1 ,n2) #:when (and (number? n1) (number? n2)) (eq? n1 n2)]
    [`(,x1 ,x2) #:when (and (symbol? x1) (symbol? x2))
      (define maybe-value (dict-ref env x1 #f))
      (if maybe-value
        (eq? x1 maybe-value)
        (eq? x1 x2))]
    [`((let (x1 e1-named) e1-body) (let (x2 e2-named) e2-body))
      (alpha-equivalent? e1-body e2-body
      (rename context xs1 xs2))]
    [_ #f]))
  (alpha-equivalent? input-e1 input-e2 '()))

(module+ test+
  (require rackunit)

  (check-false
   (alpha-equivalent?
    'y
    'x))
  
  (check-true
    (alpha-equivalent? 'x 'x))

  (check-true
   (alpha-equivalent?
    '(let ([x 5]) x)
    '(let ([y 5]) y))))
