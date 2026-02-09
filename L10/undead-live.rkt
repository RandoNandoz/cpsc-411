#lang racket

;; TODO: Perform the undead-analysis for the following

(define eg1
  '(module
     ((locals (x.1 y.2)))
     (begin
       (set! y.1 1)
       (set! x.1 1)
       (halt y.1))))

(define ust1 
  '(
    (y.1)
    (y.1)
    ()))

(define eg2
  '(module
     ((locals (x.1 y.2)))
     (begin
       (set! x.1 1)
       (set! y.1 1)
       (halt y.1))))

(define ust2 
  '(
    ()
    (y.1)
    ()))

(define eg3
  '(module
     ((locals (x.1 y.2)))
     (begin
       (set! x.1 1)
       (begin
         (set! y.1 1))
       (halt y.1))))

(define ust3 
  '(
    ()
    (
     (y.1))
    ()))

(define eg4
  '(module
     ((locals (x.1 y.2)))
     (begin
       (set! y.2 1)
       (set! x.1 2)
       (set! y.2 (* y.2 x.1))
       (begin
         (set! x.1 (+ x.1 -1))
         (set! y.2 (* y.2 x.1))
         (begin
           (set! x.1 (+ x.1 -1))
           (set! y.2 (* y.2 x.1))))
       (halt y.2))))

;; skeleton of above
(define ust4
  '(
    (y.2)
    (y.2 x.1)
    (y.2 x.1)
    (
     (x.1 y.2)
     (y.2 x.1)
     (
      (y.2 x.1)
      (y.2)))
    ()))

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

  ;; effect undead-set -> (values undead-set ust)
  ;; takes in the undead-out set for the current instruction
  ;; computer the undead-in set for the current instruction,
  ;; and the undead-set-tree for the effect.
  (define (analyze-effect effect undead-out)
    (match effect
      [`(begin ,effects ...)
        (for/foldr ([undead-out undead-out]
                    [ust '()])
                   ([effect effects])
          (let-values ([(undead-in new-ust)
                        (analyze-effect effect undead-out)])
            (values 
              undead-in
              (cons new-ust ust))))]
      [`(set! ,aloc_1 (,binop ,aloc_1 ,triv))
        ;; morally, remove then add aloc_1
        (let ([undead-in (set-add (set-add-triv undead-out triv) aloc_1)])
          (values undead-in undead-out))]
      [`(set! ,aloc ,triv)
        (let ([undead-in (set-add-triv (set-remove undead-out aloc) triv)])
          (values undead-in undead-out))

        #;(let* ([triv-set (analyze-triv triv)]
               [undead-in (set-union triv-set (set-remove undead-out aloc))])
          (values undead-in undead-out))

        #;(if (aloc? triv)
            (let ([undead-in (set-add (set-remove undead-out aloc) triv)])
              (values undead-in undead-out))
            (let ([undead-in (set-remove undead-out aloc)])
              (values undead-in undead-out)))]))

  ; triv -> set
  (define (analyze-triv triv)
    (if (aloc? triv)
        (list triv)
        '()))

  ; another approach:
  ; undead-set triv -> undead-set
  ; adds triv to the undead-set if the triv is an aloc.
  (define (set-add-triv undead-set triv)
    (if (aloc? triv)
        (set-add undead-set triv)
        undead-set))

  (let-values ([(_ ust) (analyze-effect effect '())])
    ust))

;; You might want racket/set
;; (set-add '() 1) = '(1)
;; (set-remove '(1 2) 1) = '(2)

(module+ test
  (require rackunit)
  (check-pred
   undead-set-tree?
   (undead-analyse-effect '(set! y.1 1)))

  (check-pred
   undead-set-tree?
   (undead-analyse-effect '(set! x.1 1)))

  (check-pred
   undead-set-tree?
   (undead-analyse-effect '(begin (set! x.1 1))))

  (check-pred
   undead-set-tree?
   (undead-analyse-effect '(begin (set! y.1 1) (set! x.1 1))))

  (check-equal?
   (undead-analyse-effect '(begin (set! y.1 1) (set! x.1 1) (set! x.1 y.1)))
   (list (first ust1) (second ust1) '())))

;; ------------------------------------------------------------------------

(require (only-in cpsc411/compiler-lib aloc?))

(define (undead-set? x)
  (and (list? x) (andmap aloc? x)))

(define (undead-set-tree? ust)
  (match ust
    [x #:when (undead-set? x)
       ; (halt y.2), (set! x.1 1)
       #t]
    [`(,usts ... ,ust2)
     ;(begin effects ... effect2)
     (and (andmap undead-set-tree? usts) (undead-set-tree? ust2))]))

#;
(define (ff-for-undead-set-tree-and-tail ust tail)
  (match* (ust tail)
    [(x
      (halt ,triv))
     (... case for halt ...)]
    [(`(,ust ... ,ust_tail)
      `(begin ,effect ... ,tail))
     (... case for begin ...)
     (ff-for-undead-set-tree-and-tail ust_tail tail)]))
#;
(define (ff-for-undead-set-tree-and-effect ust effect)
  (match* (ust effect)
    [(`(,usts ... ,ust_effect)
      `(begin ,effects ... ,effect))
     (... case for begin ...)
     (map ff-for-undead-set-tree-and-effect usts effects)
     (ff-for-undead-set-tree-and-effect ust_effect effect)]
    [(ust effect)
     (... case for single instruction ...)]))

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

;; replaces dead code in tree with a nop
(define (optimize-dead-stores p)
  ;; lang -> lang-without-dead-code
  (define (opt-effect e ust)
    (match* (e ust)
      [(`(set! ,aloc ,triv) ust) (if (not (member aloc ust)) '(nop) e)]
      [(`(begin ,effects ... ,e) `(,usts ... ,ust)) `(begin ,@(map opt-effect effects usts) ,(opt-effect e ust))]
      [(_ _) e])) ; nop case

  ;; lang -> lang-without-dead-code
  (define (opt-tail tail ust)
    (match* (tail ust)
      [(`(halt ,triv) ust) tail]
      [(`(begin ,effects ... ,t) `(,usts ... ,ust)) `(begin ,@(map opt-effect effects usts) ,(opt-tail t ust))]))
  (match p
    [`(module ((locals (,alocs ...)) (undead-out ,ust)) ,tail) `(module ((locals (,alocs)) (undead-out ,ust)) ,(opt-tail tail ust))]))

(module+ test
  (require cpsc411/info-lib rackunit)

  (define (merge-eg-ust eg ust)
    (match eg
      [`(module ,info ,p)
       `(module ,(info-set info 'undead-out ust) ,p)]))

  (check-match
   (optimize-dead-stores (merge-eg-ust eg1 ust1))
   `(module ,info (begin (set! y.1 1) (nop) (halt y.1))))

  (check-match
   (optimize-dead-stores (merge-eg-ust eg2 ust2))
   `(module ,info (begin (nop) (set! y.1 1) (halt y.1))))

  (check-match
   (optimize-dead-stores (merge-eg-ust eg3 ust3))
   `(module ,info (begin (nop) (begin (set! y.1 1)) (halt y.1))))

  (check-match
   (optimize-dead-stores (merge-eg-ust eg4 ust4))
   `(module
        ,info
      (begin
        (set! y.2 1)
        (set! x.1 2)
        (set! y.2 (* y.2 x.1))
        (begin
          (set! x.1 (+ x.1 -1))
          (set! y.2 (* y.2 x.1))
          (begin
            (set! x.1 (+ x.1 -1))
            (set! y.2 (* y.2 x.1))))
        (halt y.2)))))

;; Local Variables:
;; eval: (put 'module 'racket-indent-function 0)
;; End:
