#lang at-exp racket

(module+ test
  (require rackunit))

;; The point of this is three fold:
;; 1. To introduce Racket operations over trees and strings.
;; 2. To introduce a basic functional compiler design
;; 3. To introduce how assumptions about the source language influence the design and implementation of the compiler.

;; TODO: EXERCISE 1
;; Write a compiler that consumes the following language and produces a string representing an x64 program.
;;
;; p ::= (begin
;;         (define (fact n r)
;;           (if (zero? n)
;;               r
;;               (let ([r (* r n)])
;;                 (let ([n (sub1 n)])
;;                   (fact n r)))))
;;         (define (main)
;;           (fact 5 1)))
;;
;; The language is very restricted, but your compiler should be designed to handle future extensions allowing more definitions and expressions.

;; We'll first decompose the syntax into the following
;;
;; p   ::= (begin def ...)
;; def ::= (define (name x ...) e)
;; e   ::= x | (if e e1 e2) | (zero? x) | (sub1 x) | (* x x) | (let ([x e]) e) | (x e ...) | number
;;
;; with the understanding that there are some restrictions we can rely on and exploit

(define (exit-sys-call)
  (match (system-type)
    ['macosx
     "  mov rax, 0x2000001"]
    [_
     "  mov rax, 60"]))

(define registers '(r8 r9 r10))
(define return-register 'rdi)
(define return-label-register 'rax)

(define (compile-program p)
  (match p
    [`(begin ,d ...)
      (~a #:separator "\n"
        ;; install boilerplate
        "global main"
        "section .text"

        ""
        (format "mov ~a, exit" return-label-register)
        "jmp main"
        (apply string-append (map compile-def d))

        ;; install run-time system
        "exit:"
        (exit-sys-call)
        "  syscall"

        ;; install boilerplate for macOS
        "section .data"
        "  dummy: db 0")]))

(define (compile-def d)
  "")

(define (compile-expr env e k)
  "")

(module+ test
  (check-match
    (compile-expr '() 0 (lambda (x) (format "mov r9, ~a" x)))
    "mov r9, 0")

  (check-match
    (compile-expr '((x . r8) (r . r9) (n . r10))
                  `(if (zero? x)
                       r
                       (fact r n))
                  (lambda (x)
                    (~a #:separator "\n"
                        (format "mov ~a, ~a" return-register x)
                        (format "jmp ~a" return-label-register))))
    (regexp #px"\\s+cmp r8, 0\\s+je \\w+\\s+mov r9, r10\\s+mov r8, r9\\s+jmp fact\\s+\\w+:\\s+mov (\\w+), r9\\s+jmp (\\w+)"
            (list _ ret-val-reg? ret-label-reg?))
    (and (eq? (string->symbol ret-val-reg?) return-register)
         (eq? (string->symbol ret-label-reg?) return-label-register)))

  (displayln
    (compile-program
      `(begin
         (define (fact n r)
           (if (zero? n)
               r
               (let ([r (* r n)])
                 (let ([n (sub1 n)])
                   (fact n r)))))
         (define (main)
           (fact 5 1))))))
