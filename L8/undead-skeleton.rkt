#lang racket

;; TODO: Perform the undead-analysis for the following

(define eg1
  '(module ((locals (x.1 y.2)))
           (begin
             (set! y.1 1) ; undead-in (y.1) undead-out () because we set! y.1
             (set! x.1 1) ; undead-in: (y.1),
             (halt y.1)) ; undead-out: ()
     ))

(define ust1 '())

(define eg2
  '(module ((locals (x.1 y.2)))
           (begin
             (set! x.1 1)
             (set! y.1 1)
             (halt y.1))
     ))

(define ust2 '())

(define eg3
  '(module ((locals (x.1 y.2)))
           (begin
             (set! x.1 1)
             (begin
               (set! y.1 1))
             (halt y.1))
     ))

(define ust3 '())

(define eg4
  '(module ((locals (x.1 y.2)))
           (begin
             (set! y.1 1)
             (set! x.1 2)
             (set! y.2 (* y.2 x.1))
             (begin
               (set! x.1 (+ x.1 -1))
               (set! y.2 (* y.2 x.1))
               (begin
                 (set! x.1 (+ x.1 -1))
                 (set! y.2 (* y.2 x.1))))
             (halt y.2))
     ))

;; skeleton of above
(define ust4 '(() () () (() () (() ())) ()))

;; ------------------------------------------------------------------------

;; TODO: *Design* the following function that performs undead analysis on a
;; single effect.
;; You do not need to finish the implementation.

;; effect ::= (begin effect ...)
;;         | (set! aloc triv)
;;         | (set! aloc_1 (binop aloc_1 triv))

;; triv ::= aloc | int64

;; effect -> undead-set-tree?
(define (undead-analyse-effect effect)
  (void))

(module+ test
  (require rackunit)
  (check-pred undead-set-tree? (undead-analyse-effect '(set! y.1 1)))

  (check-pred undead-set-tree? (undead-analyse-effect '(set! x.1 1)))

  (check-pred undead-set-tree?
              (undead-analyse-effect '(begin
                                        (set! x.1 1))))

  (check-pred undead-set-tree?
              (undead-analyse-effect '(begin
                                        (set! y.1 1)
                                        (set! x.1 1))))

  (check-equal? (undead-analyse-effect '(begin
                                          (set! y.1 1)
                                          (set! x.1 1)
                                          (set! x.1 y.1)))
                (list (first ust1) (second ust1) '())))

;; ------------------------------------------------------------------------

(require (only-in cpsc411/compiler-lib aloc?))

(define (undead-set? x)
  (and (list? x) (andmap aloc? x)))

(define (undead-set-tree? ust)
  (match ust
    [x
     #:when (undead-set? x)
     ; (halt y.2), (set! x.1 1)
     #t]
    ;(begin effects ... effect2)
    [`(,usts ... ,ust2) (and (andmap undead-set-tree? usts) (undead-set-tree? ust2))]))

#;(define (ff-for-undead-set-tree-and-tail ust tail)
    (match (cons ust tail)
      [(cons x `(halt ,triv)) (... case for halt ...)]
      [(cons `(,ust ... ,ust_tail)
             `(begin
                ,effect ...
                ,tail))
       (... case for begin ...)
       (ff-for-undead-set-tree-and-tail ust_tail tail)]))
#;(define (ff-for-undead-set-tree-and-effect ust effect)
    (match (cons ust effect)
      [(cons `(,usts ... ,ust_effect)
             `(begin
                ,effects ...
                ,effect))
       (... case for begin ...)
       (map ff-for-undead-set-tree-and-effect usts effects)
       (ff-for-undead-set-tree-and-effect ust_effect effect)]
      [(cons ust effect) (... case for single instruction ...)]))

;; ------------------------------------------------------------------------

;; TODO: Implement the following optimization pass, which uses the undead set
;; tree to replace definitions of dead variables with '(nop), a no-op instruction.

;; Lang:
;;
;; p      ::= (module info tail)
;; info   ::= ((locals (aloc ...)) (undead-out ust?))
;; tail   ::= (begin effect ... tail) | (halt triv)
;; effect ::= (begin effect ... effect) | (set! aloc_1 (binop aloc_1 triv)) | (nop)
;; triv   ::= integer | aloc?

(require cpsc411/info-lib)

(define (optimize-dead-stores p)
  (void))

(module+ test
  (require cpsc411/info-lib
           rackunit)

  (define (merge-eg-ust eg ust)
    (match eg
      [`(module ,info ,p
          )
       `(module ,(info-set info 'undead-out ust) ,p
          )]))

  (check-match (optimize-dead-stores (merge-eg-ust eg1 ust1))
               `(module ,info
                        (begin
                          (set! y.1 1)
                          (nop)
                          (halt y.1))
                  ))

  (check-match (optimize-dead-stores (merge-eg-ust eg2 ust2))
               `(module ,info
                        (begin
                          (nop)
                          (set! y.1 1)
                          (halt y.1))
                  ))

  (check-match (optimize-dead-stores (merge-eg-ust eg3 ust3))
               `(module ,info
                        (begin
                          (nop)
                          (begin
                            (set! y.1 1))
                          (halt y.1))
                  ))

  (check-match (optimize-dead-stores (merge-eg-ust eg4 ust4))
               `(module ,info
                        (begin
                          (set! y.1 1)
                          (set! x.1 2)
                          (set! y.2 (* y.2 x.1))
                          (begin
                            (set! x.1 (+ x.1 -1))
                            (set! y.2 (* y.2 x.1))
                            (begin
                              (set! x.1 (+ x.1 -1))
                              (set! y.2 (* y.2 x.1))))
                          (halt y.2))
                  )))

;; Local Variables:
;; eval: (put 'module 'racket-indent-function 0)
;; End:
