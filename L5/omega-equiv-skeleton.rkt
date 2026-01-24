#lang racket

;; TODO EXERCISE 3: Prove that the following three programs are not equivalent, or
;; argue that they are.

(define (loop1) (let loop () (loop)))
(define (loop2) (loop2))
(define (omega) ((lambda (x) (x x)) (lambda (x) (x x))))

(define (f g)
  (set! loop1 67)
  (set! loop2 67)
  (set! omega 67)
  (g))

(module+ test
  (require rackunit)
  (check-equal?
   (f loop1)
   (f omega))

  (check-equal?
   (f loop2)
   (f omega)))
