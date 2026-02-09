#lang racket

;; An Atom is not a list
(define atom? (compose not list?))

;; A Tree is one of:
;; - An Atom
;; - (list Tree ...), i.e., a list of Trees

;; That is to say, a Tree is:
;; t ::= v | (list t ...)
;; v ::= atom?

;; Two Trees are similar if they have identical structure, but possibly unequal leaf nodes

;; EXERCISE: Design and implement tree-similar?
(define (tree-similar? t1 t2)
  (void))

(module+ test
  (require rackunit)
  (check-true (tree-similar? '() '()))
  (check-false (tree-similar? '() '(1)))
  (check-true (tree-similar? '(1) '(1)))
  (check-true 
    (tree-similar? 
      '((1 2) 
        ((3 (4 5)) 
         6) 
        7) 
      '((9 8) 
        ((7 (6 5)) 
         4) 
        3))))

;; EXERCISE: Design and implement merge-tree.
;; Given two similar trees, merge them into a single tree, so that all leaf nodes are sets.
;;
;; The output should match `t` in the following grammar
;;
;; t ::= s | (list t ...)
;; s ::= (set v ...)
;; v ::= integer? | string? | symbol? | ...
(define (merge-tree t1 t2)
  (void))

(module+ test
  (check-equal?
    (merge-tree '() '())
    '())

  (check-equal?
    (merge-tree '(1) '(2))
    `(,(set 1 2))))

;; EXERCISE: Design and implement tree-diff.
;; Given two trees, output a new tree representing the difference between the two trees 
;;
;; The output should match `d` in the following grammar
;;
;; d ::= l | (list d ...)
;; l ::= '() | `(add ,t) | `(delete ,t) | `(change ,t1 ,t2)
;; t ::= v | (list v ...)
;; v ::= integer? | string? | symbol? | ...
(define (tree-diff t1 t2)
  (void))

(module+ test
  (check-equal?
    (tree-diff '(1) '(2))
    `((change 1 2)))

  (check-equal?
    (tree-diff '(1) '(1 2))
    `(() (add 2)))

  (check-equal?
    (tree-diff '(1 2) '(1))
    `(() (delete 2))))
