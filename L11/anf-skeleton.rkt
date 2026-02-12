#lang racket

(require
 cpsc411/compiler-lib)

#|

int is an integer literal

Source-lang:

e   ::= int | (* e e) | (zero? e) | (if e e e) 

ANF-Lang:

p    ::= tail
tail ::= n | (let* ([x n] ...) tail) | (if n tail tail)
n    ::= (* v v) | (zero? v) | v
v    ::= int | x

|#

;; EXERCISE 1: Implement the ANF translation.
;; 
;; Source-lang -> ANF-Lang
(define (anf-translate e)

  ;; Source-lang.n -> (values (Listof ))

  ;; Source-lang.e -> (values (Listof `(x ANF-Lang.n)) ANF-Lang.n)
  ;; Returns a list of bindings in reverse order and an `n`.
  (define (translate-e e)
    (match e
      [`(* ,e1 ,e2)
        (define x1 (gensym))
        (define x2 (gensym))
        (let-values ([(bs1 n1) (translate-e e1)]
                     [(bs2 n2) (translate-e e2)])
          (values (append bs2 bs1 (list `(,x2 ,n2) `(,x1 ,n1)))) `(* ,x1 ,x2))]
      [`(zero? ,e)
        (define x (gensym))
        (let-values ([(bs n) (translate-e e)])
          (values
            (append bs (list `(,x ,n)))
            `(zero? ,x)))]
      [`(if ,pred ,e1 ,e2)
        (let-values ([(bsp np) (translate-e pred)]
                     [(bs1 n1) (translate-e e1)]
                     [(bs2 n2) (translate-e e2)])
          (values bsp `(if ,np (anf-translate e1) (anf-translate e2))))]
      [n #:when (number? n) (values empty n)]))

  (let-values ([(bs n) (translate-e e)])
    ;; need ANF-Lang.tail
    `(let* ,(reverse bs) ,n)))

; run with raco test -s eg1 anf-skeleton.rkt
(module+ eg1
  (anf-translate '(* (* 3 4) (* 5 6)))
  (anf-translate '(if (zero? 0) (* 3 4) (* 5 6)))
  (anf-translate '(* 6 (* 3 (if (zero? (if (zero? 0) 4 5))
                                8 9)))))

(module+ test
  (require rackunit)
  ; fragile tests
  (check-match
    (anf-translate '(* (* 3 4) (* 5 6)))
    `(let* ([,x1 3]
            [,x2 4]
            [,x3 5]
            [,x4 6]
            [,x5 (* ,x1 ,x2)]
            [,x6 (* ,x3 ,x4)])
       (* ,x5 ,x6)))

  (check-match
    (anf-translate '(if (zero? 0) (* 3 4) (* 5 6)))
    `(let* ([,x1 0])
       (if (zero? ,x1)
           (let* ([,x2 3]
                  [,x3 4])
             (* ,x2 ,x3))
           (let* ([,x4 5]
                  [,x5 6])
             (* ,x4 ,x5))))))

;; EXERCISE 2: Implement the ANF translation, without passing a list of bindings.
;; 
(define (anf-translate^ e)
  ;; Source-lang.e ...  -> ANF-Lang.tail
  (define (translate-tail tail k)
    (void))

  (translate-tail e (lambda (x) x)))

; run with raco test -s eg1 anf-skeleton.rkt
(module+ eg2
  (anf-translate^ '(* (* 3 4) (* 5 6)))
  (anf-translate^ '(if (zero? 0) (* 3 4) (* 5 6)))
  (anf-translate^ '(* 6 (* 3 (if (zero? (if (zero? 0) 4 5))
                                8 9)))))
