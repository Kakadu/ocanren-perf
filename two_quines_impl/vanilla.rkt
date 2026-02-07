#lang racket ;; RACKET

(require pretty-format)

(require "./mk.rkt")

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

(define unif-counter 0)
(define ===
  (lambda (x y)
    (lambda (s)
      (begin
        (set! unif-counter (add1 unif-counter))
        ((== x y) s)))))

(define =//= (lambda (x y) (lambda (s) ((=/= x y) s))))
(include "q_vanilla.scm")

(define count
  (let ([data (getenv "COUNT")])
    (match data
      [#f 2]
      [s (string->number data)])))

(list-display (run count (p) (eval-expo p '() p)))
(pretty-printf "~a unifications\n" unif-counter)
