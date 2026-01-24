#lang racket

#|
Source:

e ::= (let ([x e]) e) | (if e e e) | (binop e e) | integer? | x

|#


;; TODO 1: Warm up. Implement an interpreter for Source.
;; Challenge mode: do it in as few lines as possible (while still using well designed/well formatted code).
;; Source -> Number
;; Env is

;; TODO 1: Warm up. Implement an interpreter for Source.
;; Challenge mode: do it in as few lines as possible (while still using well designed/well formatted code).
(define (interp-source e)
  (define (interp-source--env e env)
    (match e
      [`(let ([,x ,e1] ,e2))
       (interp-source--env e2 (dict-set env x (interp-source--env e1 env)))]
      [`(if ,e1 ,e2 ,e3)
       (if (not (zero? (interp-source--env e1 env)))
           (interp-source--env e2 env)
           (interp-source--env e3 env))]
      [`(,op ,e1 ,e2)
       ((eval op)
        (interp-source--env e1 env)
        (interp-source--env e2 env))]
      [n #:when (number? n) n]
      [x (dict-ref env x)]))
  (interp-source--env e (hash)))

; (define interp-source eval)


#|
Monadic Lang:

V ::= integer? | x
N ::= V | (binop V V)
C ::= N | (let ([x C]) C) | (if V C C)

|#

;; TODO 2: Implement the monadic form transformation for Source.
;; Source -> Monadic Lang
(define (monadic-form e)
  (let loop ([e e])
    (match e
      [`(let ([,x ,e1] ,e2)) `(let ([x ,(loop e1)]) ,(loop e2))]
      [`(if ,e1 ,e2 ,e3)
       (let ([x (gensym)])
         `(let ([,x ,(loop e1)])
            (if ,x ,(loop e2) ,(loop e3))))
       ]
      [`(,op ,e1 ,e2) (let* ([x1 (gensym)]
                             [x2 (gensym)])
                        `(let ([,x1 ,(loop e1)])
                           (let ([,x2 ,(loop e2)])
                             (,op ,x1 ,x2))))]
      [n #:when (number? n) n]
      [x x])))

(module+ test
  (require rackunit)

  ;; NOTE: Fragile syntactic tests, might be wrong
  (check-match
   (monadic-form 5)
   5)

  (check-match
   (monadic-form `(+ 5 6))
   `(let ([,x 5]) (let ([,y 6]) (+ ,x ,y))))
  )

#|
Statement Lang:

V ::= integer? | x
N ::= V | (binop V V)
C ::= N | (set! x N) | (begin C ... C) | (if V C C)

|#

;; TODO 3: Implement statementify to translate Monadic Lang into Statement Lang.
; (define (statementify C)
;   (match C
;     [`(let ([,x1 ,c1] ,c2))
;       `(begin
;         (set! ,x1 ,(statementify c1))
;         ,(statementify c2))]
;     [`(,op ,v1 ,v2) C]
;     [`(if v c1 c2) C]
;     [x x]
;     [n #:when (number? n) n]))

(define (statementify C)
  (define (statementify-V V) V)
  (define (statementify-N N) N)
  (define (statementify-C C)
    (match C
      [`(let ([,x ,C1]) ,C2)
       `(begin
          (set! ,x ,(statementify-C C1))
          ,(statementify-C C2))]
      [`(if ,V ,C1 ,C2)
       `(if ,(statementify-V V)
            ,(statementify-C C1)
            ,(statementify-C C2))]
      [_ (statementify-N C)]))
  (statementify-C C))
(module+ test
  ;; NOTE: Fragile syntactic tests; might be wrong
  (check-match
   (statementify '(let ([x 5]) x))
   `(begin (set! ,x 5) ,x))

  (check-match
   (statementify '(let ([x (+ 5 6)]) x))
   `(begin (set! ,x (+ 5 6)) ,x))

  (check-match
   (statementify '(let ([x (+ 5 6)]) (if 0 (+ x 5) (let ([y (+ y 6)]) (+ y 7)))))
   `(begin (set! ,x (+ 5 6)) (if 0 (+ ,x 5) (begin (set! ,y (+ ,y 6)) (+ ,y 7)))))

  )

;; Now let's actually test the compiler is correct.

;; TODO 4: Implement an interpreter for Monadic, in 1 line of code.
(define (interp-monadic e)
  (void))

;; TODO 5: Implement the interpreter for Statement Lang.
(define (interp-statement e)
  (void))

(module+ test
  (check-equal?
   (interp-source '(let ([x (+ 5 6)]) (if 0 (+ x 5) (let ([y (+ y 6)]) (+ y 7)))))
   (interp-statement (statementify '(let ([x (+ 5 6)]) (if 0 (+ x 5) (let ([y (+ y 6)]) (+ y 7))))))))

;; TODO 6: Modify the design of statement lang so that it has no top-level
;; expressions.
;;
;; That is, the following would no longer be valid:
;;   (begin (set! x (+ 4 5)) x)
;; and would have to be written
;;   (begin (set! x (+ 4 5)) (halt x))

;; TODO 7: Update statementify to produce the new language; you should modify
;; exactly 1 line of code.

;; TODO 8: Update interp-statement so that the main loop never returns.
