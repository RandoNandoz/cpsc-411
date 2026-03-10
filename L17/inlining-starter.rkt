#lang racket

;; An introduction to procedure inlining following a simplification of GHC's
;; inliner from:
;; https://www.microsoft.com/en-us/research/wp-content/uploads/2002/07/inline.pdf
;;
;; This version is a simplification in that:
;; 1. It picks loop breakers naively
;; 2. It ONLY inlines procedures (not other bound expressions)
;; 3. All calls are direct, so it's not context sensitive (although the
;;    implementation leaves some hooks and comments about where context sensitivity
;;    would be added)
;;
;; It's tailored to Values-lang-v6 from my CPSC 411 course.

;; Starting from Section 5 of the paper.

(require
  (prefix-in r: graph) ; raco pkg install graph
  cpsc411/compiler-lib
  cpsc411/graph-lib
  "uniquify.rkt"
  (prefix-in s: "../langs/v6/values-lang-v6.rkt")
  (prefix-in t: "../langs/v6/values-unique-lang-v6.rkt"))

(module+ test
  (require rackunit))

;; ------------------------------------------------------------------------
;; Helpers
;; ------------------------------------------------------------------------

;; A macro for writing templates, so the "...." raises an error.
(define-syntax (.... stx)
  #`(error 'template "Incomplete template at line ~a" #,(syntax-line stx)))

;; map f over ls with an extra accumulator.
;;
;; (A B -> (values C B)) B (listof A) -> (values (listof C) B)
(define (monadic-map f init ls)
  (for/foldr ([ss '()]
              [r init])
    ([x ls])
    (let-values ([(s e) (f x r)])
      (values (cons s ss) e))))

;; compute the strongly connected components of a functional Graph
;;
;; Graph -> (listof (listof Node))
(define (scc g)
  (define (fg->g fg)
    (let ([g (r:directed-graph '())])
      (for-each (curry r:add-vertex! g) (map car fg))
      (for ([v (map car fg)])
        (for ([u (get-neighbors fg v)])
          (r:add-directed-edge! g v u)))
      g))
  (r:scc (fg->g g)))

;; ------------------------------------------------------------------------
;; Procedure Inlining
;; ------------------------------------------------------------------------

;; TODO 0: replace with your own implementation of uniquify
(define (uniquify x)
  (t:ast->sexpr (uniquify-typed (s:sexpr->ast x))))

#|
Source/Target language.

Values-unique-lang:

p      ::= (module (define label (lambda (aloc ...) value)) ... value)
pred   ::= ....
value  ::= triv | (let ([aloc value] ...) value) | (call label triv ...)
           (binop triv triv) | (if pred value value)
triv   ::= aloc | integer | label
binop ::= - | + | *
|#


#|
First IL

Values-unique-lang/dependencies:

p      ::= (module info (define label info (lambda (aloc ...) value)) ... value)
info   ::= (('deps deps) any/c ...)
deps   ::= (label ...)

pred   ::= ....
value  ::= triv | (let ([aloc value] ...) value) | (call label triv ...)
           (binop triv triv) | (if pred value value)
triv   ::= aloc | integer | label
binop ::= - | + | *
|#

;; Values-unique-lang -> Values-unique-lang/dependencies
;; TODO 1: implement dependency analysis
(define (analyze-dependencies p)
  (define (analyze-p p)
    (match p
      [`(module ,defs ... ,value)
       `(module ((deps ,(analyze-value value empty))) ,@(map transform-def defs),value)]))

  ;; unique-v6-def -> unique-v6/deps-def
  (define (transform-def def)
    (match def
      [`(define ,label (lambda (,aloc ...) ,value))
       `(define ,label ((deps ,(analyze-def def))) (lambda (,@aloc) ,value))]))

  ;; Values-unique-lang-def -> (listof Label)
  (define (analyze-def def)
    (match def
      [`(define ,label (lambda (,aloc ...) ,value))
       (analyze-value value empty)]))

  ;; Values-unique-lang-value -> 
  (define (analyze-value value deps)
    (match value
      [`(let ([,named ,vals] ...) ,body-value) (append (foldl analyze-value deps vals) (analyze-value body-value deps))]
      [`(call ,label ,triv) (cons label (analyze-triv triv deps))]
      [`(binop ,triv1 ,triv2) (append (analyze-triv triv1 deps) (analyze-triv triv2 deps))]
      [`(if ,pred ,val1 ,val2) (append (analyze-pred pred deps) (analyze-value val1 deps) (analyze-value val2 deps))]
      [triv (analyze-triv triv deps)]))

  (define (analyze-pred pred deps)
    (match pred
      [`(let ([,alocs ,vals] ...) ,pred-body) (append (foldl analyze-value deps vals) (analyze-pred pred-body deps))]
      [`(not ,pred-inner) (analyze-pred pred-inner deps)]
      [`(if ,cnd ,thn ,els) (append (analyze-pred cnd deps) (analyze-pred thn deps) (analyze-pred els deps))]
      [_ deps]))

  (define (analyze-triv triv deps)
    (if (label? triv) (cons triv deps) deps))

  (analyze-p p))

(module+ test
  ;; fact depends on itself, and main depends on fact
  (check-match
   (analyze-dependencies
    (uniquify
     '(module
          (define fact
            (lambda (x)
              (if (= 0 x)
                  1
                  (let ([n (- x 1)])
                    (let ([r (call fact n)])
                      (* x r))))))
        (call fact 5))))
   `(module
        ((deps (,fact)))
      (define ,fact ((deps (,fact))) ,tail1)
      ,tail2))

  ;; id doesn't depend on itself, but main depends on it
  (check-match
   (analyze-dependencies
    (uniquify
     '(module
          (define id
            (lambda (x)
              x))
        (call id 5))))
   `(module
        ((deps (,id)))
      (define ,id ((deps ())) ,tail1)
      ,tail2))

  ;; odd depends on even, even depends on odd, and main depends on even
  (check-match
   (analyze-dependencies
    (uniquify
     '(module
          (define odd?
            (lambda (x)
              (if (= x 0)
                  0
                  (let ([y (- x 1)])
                    (call even? y)))))
        (define even?
          (lambda (x)
            (if (= x 0)
                1
                (let ([y (- x 1)])
                  (call odd? y)))))
        (call even? 5))))
   `(module
        ((deps (,even?)))
      (define ,odd? ((deps (,even?))) ,tail1)
      (define ,even? ((deps (,odd?))) ,tail2)
      ,tail3)))
#|
Second IL

Values-unique-lang/o-map:

p      ::= (module info (define label info (lambda (aloc ...) value)) ... value)
info   ::= (o-map any/c ...)
o-map  ::= ((label flag) ...)
flag   ::= 'loop-breaker | 'dead | 'once-safe | 'multi-safe | 'once-unsafe | 'multi-unsafe

pred   ::= ....
value  ::= triv | (let ([aloc value] ...) value) | (call label triv ...)
           (binop triv triv) | (if pred value value)
triv   ::= aloc | integer | label
binop ::= - | + | *
|#

;; Values-unique-lang/dependencies -> Values-unique-lang/o-map
(define (occurrence-analysis p)
  (define (analyze-p p)

    ;; TODO 2a: implement loop-breaking
    ;; Assume all labels are dead, to start with

    ;; Next, mark loop breakers

    ;; TODO 2a: implement occurrence-analysis for non-loops
    ;; Then, do occurrence analysis
    ....)

  (define (analyze-def def)
    ....)

  (define (analyze-value value)
    ....)

  (define (analyze-pred pred)
    ....)

  (define (analyze-triv triv)
    ....)

  (analyze-p p))

(module+ test
  ;; breaks self loops
  (check-match
   (occurrence-analysis
    (analyze-dependencies
     (uniquify
      '(module
           (define loop
             (lambda (x)
               (call loop x)))
         (call loop 0)))))
   `(module
        ,o-map
      (define ,loop ,tail1)
      ,tail2)
   (and
    (eq? 'loop-breaker (info-ref o-map loop))))

  ;; should pick some loop breaker, arbitrarily.
  ;; if odd? is the loop-breaker, then even? is multi-unsafe
  ;; otherwise, even? is the loop-breaker, and odd? is once-unsafe
  (check-match
   (occurrence-analysis
    (analyze-dependencies
     (uniquify
      '(module
           (define odd?
             (lambda (x)
               (if (call eq? x 0)
                   0
                   (let ([y (call + x -1)])
                     (call even? y)))))
         (define even?
           (lambda (x)
             (if (call eq? x 0)
                 1
                 (let ([y (call + x -1)])
                   (call odd? y)))))
         (call even? 5)))))
   `(module
        ,o-map
      (define ,odd? ,tail1)
      (define ,even? ,tail2)
      (call ,even? 5))
   (and
    (cond
      [(eq? 'loop-breaker (info-ref o-map odd?))
       (eq? 'multi-unsafe (info-ref o-map even?))]
      [(eq? 'loop-breaker (info-ref o-map even?))
       (eq? 'once-unsafe (info-ref o-map odd?))]
      [else #f]))))

(define current-inline-threshold (make-parameter 100))

;; Values-unique-lang/o-map -> Values-unique-lang
;; TODO: Implement inlining
(define (simplify p)
  (define (simplify-p p)
    (match p
      [`(module ,info ,defs ... ,value)
       ....]))

  (define (simplify-def def)
    ....)

  (define (simplify-value value)
    ....)

  (define (simplify-pred pred)
    ....)

  (define (simplify-triv triv)
    ....)

  (simplify-p p))

(module+ test
  ;; FRAGILE syntactic test
  (check-match
   (simplify
    (occurrence-analysis
     (analyze-dependencies
      (uniquify
       '(module
            (define id
              (lambda (x)
                x))
          (call id 5))))))
   `(module ,defs ... (let ([,x 5]) ,x))))

;; Takes a procedure f and a bound, and produces a procedure that that runs f on
;; itself up to that bound.
;;
;; (A -> A) positive-integer? -> (A -> A)
(define ((until-fix f [bound 5]) p)
  (let/ec k
    (let loop ([p p]
               [n bound])
      (when (zero? n)
        (k p))
      (let ([new-p (f p)])
        (if (equal? new-p p)
            new-p
            (loop new-p (sub1 n)))))))

;; Values-unique-lang -> Values-unique-lang
(define simplify-exprs (compose simplify occurrence-analysis analyze-dependencies))
#;(define (simplify-exprs x)
    (simplify (occurance-analysis (analyze-dependenciex x))))

(module+ test
  ;; FRAGILE syntactic test
  (check-match
   ((until-fix simplify-exprs)
    (uniquify
     '(module
          (define id
            (lambda (x)
              x))
        (call id 5))))
   `(module (let ([,x 5]) ,x)))

  ;; One of the procedures should have been completely inlined
  ;; could have been even or odd
  (check-match
   ((until-fix simplify-exprs)
    (uniquify
     '(module
          (define odd?
            (lambda (x)
              (if (= x 0)
                  0
                  (let ([y (- x 1)])
                    (call even? y)))))
        (define even?
          (lambda (x)
            (if (= x 0)
                1
                (let ([y (- x 1)])
                  (call odd? y)))))
        (call even? 5))))
   `(module
        (define ,even-or-odd? ,tail1)
      ,tail2)))

