#lang racket

;; TODO EXERCISE 1:
;; Are sum1 and sum2 equivalent? If not, prove it. If so, argue why they must
;; be.
;;
;; A proof of inequivalence, that is, a counter example to equivalence, is a
;; single program that would return different values when given the supposed
;; equivalent programs.
;; For example, a definition of the following `f` that causes its test to fail
;; would be a counter example.

(define (f sum)
  ;; TODO: Redefine to prove that `sum1` and `sum2` are not equivalent, if you
  ;; believe they are not.

  (set! rsf 67)
  (sum '()))

(define rsf 0)
(define (sum1 ls)
  (if (empty? ls)
      rsf
      (begin
        (set! rsf (+ rsf (car ls)))
        (sum1 (cdr ls)))))

(define (sum2 ls)
  (define rsf 0)
  (let sum2 ([ls ls])
    (if (empty? ls)
        rsf
        (begin
          (set! rsf (+ rsf (car ls)))
          (sum2 (cdr ls))))))

;; They must be equal

(module+ test
  (require rackunit)
  (check-equal?
   (f sum1)
   (f sum2)))

;; TODO EXERCISE 2:
;; Are sum2 and sum3 equivalent? If not, prove it. If so, argue why they must be.

(define (g sum)
  ;; TODO: Redefine to prove that `sum2` and `sum3` are not equivalent, if you
  ;; believe they are not.
  (sum '(1 2 3 4 5)))

(define (sum3 ls)
  (apply + ls))

(module+ test
  (check-equal?
   (g sum2)
   (g sum3)))
