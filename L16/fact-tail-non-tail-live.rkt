#lang racket

(define (fact x)
  (if (zero? x)
      1
      (* x (fact (- x 1)))))

(module+ test
  (require rackunit)
  (check-equal? 120 (fact 5)))

;; ------------------------------------------------------------------------ 

;; EXERCISE 1:
;; Transform fact into an implementation using only tail-calls.
;; Without using an accumulator.

(define (fact^ x k)
  (if (zero? x)
      (k 1)
      (k (* x (let/cc return
                (fact^ (sub1 x) return)
                (error "still impossible"))))))

(define (call-fact^ x)
  (let/cc return
    (fact^ 5 return)
    (error "impossible")))

(module+ test
  (check-equal? 120 (call-fact^ 5)))

;; ------------------------------------------------------------------------ 

;; Let's go even lower-level.
;; Return is used like a function, rather than a mere jump.
;; Let's manage the data flow ourselves.

(define r7 (void))
(define r8 (void))
(define r9 (void))
(define r15 (void))
(define rax (void))

;; EXERCISE 2: implement ifact and call-ifact.
;; 
;; ifact takes no parameters and does not return, but expects arguments in some shared state, and leaves its result in some shared state.
;; Use local variables when reading from shared state, like in your compiler.
(define (ifact)
  (define x r7)
  (define k r15)
  (if (zero? x)
      (begin
        (set! rax 1)
        (k))
      (begin
        (let/cc return
          (set! r7 (- x 1))
          (set! r15 return)
          (ifact))
        (set! rax (* x rax))
        (k))))

;; Setup a call and return to ifact using its calling convention
;; should return the result of ifact.
(define (call-ifact x)
  (begin
    (void) ; TODO
    (ifact)
    (void) ; TODO
    ))

(module+ test
  (check-equal?
    120
    (call-ifact 5)))

;; ------------------------------------------------------------------------ 

;; We're still getting scope saved for us---the local variables.
;; Let's get rid of local variables entirely.

;; What do we need to get rid of local variables?

;; EXERCISE 3: implement ifact^. Do not use any local variables at all; only global shared state.
(define stack (make-vector 1000))
(define rbp 0)
(define (push! x) 
  (set! stack (cons x stack)))
(define (pop!)
  (begin0 (car stack)
    (set! stack (cdr stack))))

; (define stack^ (make-vector 1000))

(define (ifact^)
  (if (zero? r7)
      (begin
        (set! rax 1)
        (k))
      (begin
        ; (push! r7)
        (vector-set! stack rbp r7)
        (vector-set! stack (+ 1 rbp) r15)
        ; (push! r15)
        (set! rbp (+ rbp 2))
        (let/cc return
          (set! r7 (- r7 1))
          (set! r15 return)
          (ifact))
        ; (set! r15 (pop!))
        ;(set! r7 (pop!))
        (vector-ref stack (+ rbp 1) r15)
        (vector-ref stack rbp r7)
        (set! rax (* r7 rax))
        (r15))))

;; Setup a call and return to ifact^ using its calling convention
;; should return the return value of ifact^
(define (call-ifact^ x)
  (begin
    (void) ; TODO
    (ifact^)
    (void) ; TODO
    ))

(module+ test
  (check-equal?
    120
    (call-ifact^ 5)))
