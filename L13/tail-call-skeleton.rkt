#lang racket

;;------------------------------------------------------------------------
;; Some tail call magic

(require racket/control)

(define (halt x) (abort x))

(define (call f . vs)
  (apply f vs))

;;------------------------------------------------------------------------

;; TODO Exercise 1: Implement tail recursive factorial.
;; You may want to use the above halt and call statements.
(define (fact n acc)
  (if (zero? n)
      (halt acc)
      (call fact (sub1 n) (* n acc))))

(module+ test
  (require rackunit)
  (check-equal?
   (prompt (call fact 5 1))
   120)

  (check-equal?
   (prompt (call fact 6 1))
   720)

  ;; fact should not return.
  (check-equal?
   (prompt (+ 5 (call fact 5 1)))
   120)

  (check-equal?
   (prompt (begin (call fact 5 1) (displayln "Hello!!!!")))
   120)

  (check-equal?
   (prompt (begin (call fact 5 1) (error "This is impossible")))
   120))

;; Try this in a REPL
#;(begin (call fact 5 1) (displayln "Hello!"))

;; ------------------------------------------------------------------------

(require cpsc411/langs/v5)

;; TODO Exercise 2: Implement tail recursive factorial... in Paren-x64-v5.

(define (x64-fact x)
  `(begin
     ;; (with-label L.fact.1 s)

     (set! rdi ,x)
     (set! rsi 1)
     (jump L.fact.1)

     (with-label L.fact.1
       (compare rdi 0))
     (jump-if = L.fact.end)
     (set! rsi (* rsi rdi))
     (set! rdi (+ rdi -1))

     (jump L.fact.1)
     (with-label L.fact.end
       (halt rsi))))

(module+ test
  (check-equal?
   (interp-paren-x64-v5
    (x64-fact 5))
   120)

  (check-equal?
   (interp-paren-x64-v5
    (x64-fact 6))
   720))

;; ------------------------------------------------------------------------

;; EXERCISE 3:
;; Let's design and implement some calling conventions.
;;
;; 0. Pick a sequence of global variables to use as parameter/arguments.
;; 1. Replace all entries to functions with reads from the global sequence.
;; 2. Replace all calls to functions with writes to the global sequence.

(define stack (make-vector 1000))
(define rdi 0)
(define rsi 0)

;; rdi, 1st, rsi, second
;; all other params on the stack
#;
(define fact
  (lambda ()
    (if (eq? x 0)
        (halt acc)
        (call fact (- x 1) (* x acc)))))

#;
(define fact
  (lambda ()
    (begin
      (let ([x rdi]
            [acc rsi])
        (if (eq? x 0)
            (halt acc)
            (begin
              (set! rdi (- x 1))
              (set! rsi (* acc x))
              (call fact)))))))

#;
(define (f x y z acc)
  (if (eq? z 1)
    (halt acc)
    (f x y 1 (+ z x y))))

(define f
  (lambda ()
    (begin
      (let ([x rdi]
            [y rsi]
            [z (vector-ref stack 0)]
            [acc (vector-ref stack 1)])
        (if (eq? z 1)
          (halt acc)
          (begin
            (set! rdi x)
            (set! rsi y)
            (vector-set! stack 0 1)
            (vector-set! stack 1 (+ z x y))
            (call f)))))))

