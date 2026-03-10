#lang racket

(require rackunit)

;; Work in main so the debugger will display "global" variables
(define (main)

  ;; Now, we've seen how to resolve lexical scope.
  ;; What happens when we add tail calls?

  ;; TODO: step through fact together
  (define (fact x acc)
    (if (zero? x)
        acc
        (let ([acc (* x acc)])
          (let ([x (sub1 x)])
            (fact x acc)))))

  (check-equal?
   (fact 5 1)
   120)

  ;; okay, let's do the same trick

  ;; TODO: Exercise 1: convert fact into an imperative implementation using
  ;; global variables and jumps.
  ;; Do this in 3 steps, the same way your compiler does it.

  ;; TODO Step 1. ...
  (define (fact2 x.1 acc.2)
    (if (zero? x.1)
        acc.2
        (let ([acc.3 (* x.1 acc.2)])
          (let ([x.2 (sub1 x.1)])
            (fact2 x.2 acc.3)))))

  (check-equal?
   (fact2 5 1)
   120)

  (define x (void))
  (define acc (void))
  ;; TODO Step 2. ...
  (define (ifact)
    (define x.1 x)
    (define acc.2 acc)
    (define acc.3 (void))
    (define x.2 (void))
    (if (zero? x.1)
        acc.2
        (begin
          (set! acc.3 (* x.1 acc.2))
          (set! x.2 (sub1 x.1))
          (set! x x.2)
          (set! acc acc.3)
          (ifact))))

  (check-equal?
   (begin
     (set! x 5)
     (set! acc 1)
     (ifact))
   120)

  ;; TODO Step 3. Implement calling convention. Target the yolo machine, using the
  ;; following calling convention:
  ;; 1. The first argument is passed in yolo1.
  ;; 2. The second argument is passed in yolo2.
  ;; 3. The third argument is passed in yolo3.
  (define yolo1 (void))
  (define yolo2 (void))
  (define yolo3 (void))

  (define (loop-fact)
    (if (zero? yolo1)
        yolo2
        (begin
          (set! yolo2 (* yolo1 yolo2))
          (set! yolo1 (sub1 yolo1))
          (loop-fact))))

  (check-equal?
   (begin
     ;; TODO ...
     (set! yolo1 5)
     (set! yolo2 5)
     (loop-fact))
   120)

  ;; TODO Exercise 2: Register allocate, so we have only global scope.
  ;; Use the yolo machine, so you only have the three registers defined above:
  ;; yolo1, yolo2, and yolo3.

  (define (loop-fact2)
    (void))

  (check-equal?
   (begin
     (set! yolo1 5)
     (set! yolo2 1)
     (loop-fact2))
   120))

(main)
