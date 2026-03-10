#lang racket

(require rackunit)

;; Work in main so the debugger will display "global" variables
(define (main)

  ;; We'll step through this one together.
  ;; Should produce 12
  (define e1
    (let ([x 5])
      (+ (let ([x 6])
           (+ x 1))
         x)))
  (check-equal? e1 12)

  ;; TODO Exercise 1: naively translate e1 into an imperative expression
  (define x (void)) ; define a variable so it can be set
  (define ie1
    (begin
      (set! x 5)
      (set! x 6)
      (+ x (+ x 1))
      ))
  (check-equal? ie1 13)

  ;; The naive translation causes a problem. Let's fix it.

  ;; TODO Exercise 2: Rewrite e1 into e2, so that a naive translation of it
  ;; produces a correct ie2.
  (define e2
    (let ([x.1 5])
      (+ (let ([x.2 6])
           (+ x.2 1))
         x.1)))

  (check-equal? e2 12)

  ;; TODO Exercise 3: naively translate e2 into an imperative expression.
  (define x.1 (void))
  (define x.2 (void))
  (define ie2
    (begin
      (set! x.1 5)
      (set! x.2 6)
      (+ x.1 (+ x.2 1))))
  (check-equal? ie2 12)

  ;; TODO: Now, we perform register allocation to the yolo machine.
  ;; We'll compile to the yolo machine, invented by students of CPSC 411 2021w2.
  ;; The yolo machine has 3 register: yolo1, yolo2, and yolo3.
  (define yolo1 (void))
  (define yolo2 (void))
  (define yolo3 (void))
  (define ie3
    (begin
      (set! yolo1 5)
      (set! yolo2 6)
      (+ yolo1 (+ yolo2 1))))

  (check-equal? ie3 12))

(main)
