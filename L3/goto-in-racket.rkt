#lang racket

(define (f)
    5)

(let/cc k
    (k (f)))

(displayln "Hello world!")