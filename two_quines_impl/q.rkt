#lang racket

(require benchmark
         plot/pict
         racket/vector
         racket/list)
(require pretty-format)
(require macro-debugger/expand)

(require "./mk.rkt")

(define (delete-garbage term)
  (cond
    [(symbol? term) term]
    ;; (symb X)  => return X
    [(and (pair? term) (eq? (car term) 'symb)) (cadr term)]

    ;; (seq A B) => recursively process A and B
    [(and (pair? term) (eq? (car term) 'seq)) (let ([xs (cadr term)]) (map delete-garbage xs))]

    [(and (pair? term) (eq? (car term) 'quote) (eq? (cadr term) 'quote)) 'quote]

    [else (map delete-garbage term)]))

(define (list-display xs)
  (let helper ([n 1]
               [lis xs])
    (cond
      [(null? lis) #f]
      [else
       (pretty-printf "~a\n" n)
       (pretty-printf "  ~a\n" (delete-garbage (car (car lis))))
       (pretty-printf "  ~a\n" (cdr (car lis)))
       (helper (+ n 1) (cdr lis))])))

(define ===
  (lambda (x y)
    (lambda (s)
      ((== x y) s)
      ; ((==count x y) s)
      )))

(define =//= (lambda (x y) (lambda (s) ((=/= x y) s))))

(include "q_ocanren.scm")

(define count
  (let ([data (getenv "COUNT")])
    (match data
      [#f 2]
      [s (string->number data)])))

(list-display (run count (p) (eval-expo p '() `(val_ ,p))))
