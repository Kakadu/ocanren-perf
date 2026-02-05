#lang racket ;; RACKET

(require racket/vector
         racket/list
         pretty-format)

(require "./mk.rkt")

(provide eval-expo)

(define (list-display xs)
  (let helper ([n 1]
               [lis xs])
    (cond
      [(null? lis) #f]
      [else
       (pretty-printf "~a\n" n)
       (pretty-printf "  ~a\n" (car (car lis)))
       (pretty-printf "  ~a\n" (cdr (car lis)))
       (helper (+ n 1) (cdr lis))])))

(define ===
  (lambda (x y)
    (lambda (s)
      ((== x y) s)
      ; ((==count x y) s)
      )))

(define =//= (lambda (x y) (lambda (s) ((=/= x y) s))))

(include "q_vanilla.scm")

; (define count 2)

; (list-display (run count (p) (eval-expo p '() p)))
