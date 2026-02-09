#lang racket

#|
Source:

e     ::= (let ([x e]) e) | (if e e e) | (binop e e) | integer? | x
binop ::= + | * | -

Let 0 be interpreted as false, and anything else as true.
|#

;; Challenge mode: do it in as few lines as possible (while still using well designed/well formatted code).
(define (interp-source e)
  ;(eval e)
  (define (interp-source-e env e)
    (match e
      [(? symbol?)
       (dict-ref env e)]
      [(? integer?)
       e]
      [`(let ([,x ,e1]) ,e2)
        (interp-source-e 
          (dict-set env x (interp-source-e env e1))
          e2)]
      [`(if ,e ,e1 ,e2)
        (if (eq? 0 (interp-source-e env e))
            (interp-source-e env e2)
            (interp-source-e env e1))]
      [`(,binop ,e1 ,e2)
        #:when (memq binop '(+ - *))
        ((eval binop) 
         (interp-source-e env e1)
         (interp-source-e env e2))]))
  (interp-source-e '() e))

#|
Monadic Lang:

V ::= integer? | x
N ::= V | (binop V V)
C ::= N | (let ([x C]) C) | (if V C C)

|#

;; Source e -> Monadic Lang C
(define (monadic-form e)
  (let loop ([e e]
             #;[k ??])
    (match e
      [(? symbol?)
       e]
      [(? integer?)
       e]
      [`(,binop ,e1 ,e2)
        (define x1 (gensym))
        (define x2 (gensym))
       `(let ([,x1 ,(loop e1)])
          (let ([,x2 ,(loop e2)])
            (,binop ,x1 ,x2)))]
      [`(let ([,x ,e1]) ,e2)
       `(let ([,x ,(loop e1)])
          ,(loop e2))]
      [`(if ,e ,e1 ,e2)
       (define x (gensym))
       `(let ([,x ,(loop e)])
          (if ,x
              ,(loop e1)
              ,(loop e2)))])))

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
Statement-intermediate Lang:

V ::= integer? | x
N ::= V | (binop V V)
C ::= N | (set! x C) | (begin C ... C) | (if V C C) |

|#

(define (statementify C)
  (define (statementify-V V)
    V)
  (define (statementify-N N) 
    N
    #;(match N
      [`(,binop ,V1 ,V2)
        ]
      [V (statementify-V V)]))
  (define (statementify-C C k) 
    (match C
      [`(let ([,x ,C1]) ,C2)
        `(begin
           ;; TODO 1: Discuss this missing recursive call
           (set! ,x ,C1) ; why didn't I translate C1
           ,(statementify-C C2
              (lambda (N)
                `(begin
                  (set! ,x ,N)
                  ,(statementify-C C2 k)))))]
      [`(if ,V ,C1 ,C2)
       `(if ,(statementify-V V)
            ,(statementify-C C1 k)
            ,(statementify-C C2 k))]
      [_ (statementify-N C)]))
  (statementify-C C identity))

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

;; TODO 2: Implement an interpreter for Monadic, in 1 line of code.
(define (interp-monadic e)
  (interp-source e))

;; TODO 3: Update the monadic translation to avoid introducing unnecessary bindings
;; Source e -> Monadic Lang C
;; Our continuation is (V -> C)
(define (monadic-form-opt e)
  (define (monadic-V v k)
    (k v))
  (let loop ([e e]
             [k identity])
    (match e
      [(? symbol?)
       (monadic-V e k)]
      [(? integer?)
       (monadic-V e k)]
      [`(,binop ,e1 ,e2)
            (loop e1 (lambda (v1)
              (loop e2
                `(let ([x (gensym)])
                  `(begin
                    (set! ,x (lambda (v2) `(,binop ,v1 ,v2)))
                    ,(k x))))))]
      [`(let ([,x ,e1]) ,e2)
       (loop e1
        (lambda (v1)
          `(let ([,x ,v1]
              (loop e1 (lambda (v1) (loop e2 (lambda (v2) (k v2)))))))))]
      [`(if ,e ,e1 ,e2)
       (define x (gensym))
       `(let ([,x ,(loop e)])
          (if ,x
              ,(loop e1)
              ,(loop e2)))])))


;; TODO 4: Implement the interpreter for Statement Lang.
(define (interp-statement e)
  (eval e))

(module+ test
  (check-equal?
   (interp-source '(let ([x (+ 5 6)]) (if 0 (+ x 5) (let ([y (+ y 6)]) (+ y 7)))))
   (interp-statement (statementify '(let ([x (+ 5 6)]) (if 0 (+ x 5) (let ([y (+ y 6)]) (+ y 7))))))))

;; TODO 5: Modify the design of statement lang so that it has no top-level
;; expressions.
;;
;; That is, the following would no longer be valid:
;;   (begin (set! x (+ 4 5)) x)
;; and would have to be written
;;   (begin (set! x (+ 4 5)) (halt x))

;; TODO 6: Update statementify to produce the new language; you should modify
;; exactly 1 line of code.

