#lang racket

;; Implementation of a run-time system for VecVar-Lang
;; ------------------------------------------------------------------------

(define rax (void))
(define r8 (void))
(define r9 (void))
(set! rax (void))

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

;; final value is in rax

(define (vec-var? s)
  (and (symbol? s) (regexp-match? #rx"v[0-9]+" (symbol->string s))))
; v1 v19, v5

;; v1 -> 1
(define (vec-var->index s)
  (string->number (second (regexp-match #rx"v([0-9]+)" (symbol->string s)))))

(define (compile-vecvar-lang p)
  
  ;; TODO design and implement compile-vecvar-lang
  ;; p -> e
  (define (compile-p p)
    (match p
      [`(begin ,ss ... (halt ,loc))
       `(begin ,@(map compile-s ss)
               (set! rax (vector-ref memory ,(vec-var->index loc))))]))

  ;; s -> e
  (define (compile-s s)
    (match s
      [`(set! ,loc ,loc2) #:when (vec-var? loc2)
        `(vector-set! memory ,(vec-var->index loc)
                      (vector-ref memory ,(vec-var->index loc2)))]
      [`(set! ,loc ,int)
       `(vector-set! memory ,(vec-var->index loc) ,int)]))

  (compile-p p))


(module+ test
  (require rackunit)

  (begin
    (run (compile-vecvar-lang `(begin (set! v1 5) (halt v1))))
    (check-equal? rax 5))

  (begin
    (run (compile-vecvar-lang `(begin (set! v1 120) (set! v2 v1) (halt v2))))
    (check-equal? rax 120)))

(module+ debug
  ;; inspect run-time system
  (displayln rax)
  (displayln memory))
