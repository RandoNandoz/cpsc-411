#lang racket

;; Implementation of a run-time system for VecVar-Lang
;; ------------------------------------------------------------------------

(define rax (void))
(define r8 (void))
(define r9 (void))
(set! rax (void))
(set! r8 (void))
(set! r9 (void))

;; infinite memory
(define memory (make-vector 1000))

;; Black Magic
(define-namespace-anchor ns)

(define (run s)
  (eval s (namespace-anchor->namespace ns)))

;;------------------------------------------------------------------------

;; Source Language: VecVar-Lang

;; p ::= (begin s ... (halt loc))
;; s ::= (set! loc loc) | (set! loc integer)
;; loc ::= vec-var?

;; Target Language: Racket

;; e ::= ... | (begin e ...) | (vector-ref e e) | (vector-set! e e e) | (set! x e)
;;    |  rax | r8 | r9 | memory | integer

(define (vec-var? s)
  (and (symbol? s) (regexp-match? #rx"v[0-9]+" (symbol->string s))))

(define (vec-var->index s)
  (string->number (second (regexp-match #rx"v([0-9]+)" (symbol->string s)))))

(define (compile-loc loc)
  (vec-var->index loc))
;; VecVar-Lang-p -> Racket-e
;; Compiles VecVar-Lang programs to Racket
(define (compile-vecvar-lang p)
  ;; loc -> Natural
  (define (compile-s s)

    (match s
      [`(set! ,loc ,n)
       #:when (integer? n)
       `(vector-set! memory ,(compile-loc loc) ,n)]
      [`(set! ,loc1 ,loc2) `(vector-set! memory ,(compile-loc loc1) (vector-ref memory ,(compile-loc loc2)))]))
  (define (compile-p p)
    (match p
      [`(begin
          ,s ...
          (halt ,loc))
       `(begin
          ,@(map compile-s s)
          (set! rax (vector-ref memory ,(compile-loc loc))))]))
  (compile-p p))

(module+ test
  (require rackunit)

  (begin
    (run (compile-vecvar-lang `(begin
                                 (set! v1 5)
                                 (halt v1))))
    (check-equal? rax 5))

  (begin
    (run (compile-vecvar-lang `(begin
                                 (set! v1 120)
                                 (set! v2 v1)
                                 (halt v2))))
    (check-equal? rax 120)))

(module+ debug
  ;; inspect run-time system
  (displayln rax)
  (displayln memory))
