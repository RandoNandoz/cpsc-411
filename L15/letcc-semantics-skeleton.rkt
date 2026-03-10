#lang racket

;; let/cc is an expression roughly of the following syntax:
;;
;; e ::= ... | (let/cc x e ...)
;;
;; with the guarantee that x is always called in tail position.
;; You can think of it as:
;;
;; e ::= ... | (let/cc x tail)
;;
;; That is, it introduces a tail context in an arbitrary context

(module+ test
  (require rackunit)
  (check-equal?
   25
   (* 5 (let/cc return
          (return 5)
          (error "impossible")))))

;; Exercise: Implement find, which takes a list and a predicate, and returns the element that matches the predicate or #f if not found.
;; use let/cc; do not use any of the for/* abstractions or built-in higher-order list functions.
(define (find pred? ls)
  (let/cc return
    (match ls
      ['() #f]
      [(cons x r) (if (pred? x) (return x) (return (find pred? r)))])))


(define (find^ pred? ls)
  (let/cc return (foldr (lambda (x acc) (if (pred? x) (return x) acc)) #f ls)))

(module+ test
  (define x 0)
  (check-equal?
   4
   (begin
     (find (lambda (y) (set! x (+ x 1)) (zero? y))
           '(1 2 3 0 5 6 7 8))
     x))

  (check-equal?
   4
   (begin
     (find^ (lambda (y) (set! x (+ x 1)) (zero? y))
           '(1 2 3 0 5 6 7 8))
     x)))

