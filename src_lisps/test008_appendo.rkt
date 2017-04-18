#lang racket
(require racket/include)

(require "../simple-miniKanren/mk.rkt")
(include "../simple-miniKanren/test-check.scm")


(define nullo
  (lambda (x)
    (== '() x)))
(define pairo
  (lambda (p)
    (fresh (a d)
      (conso a d p))))
(define conso
  (lambda (a d p)
    (== (cons a d) p)))
(define poso
  (lambda (n)
    (fresh (a d)
      (== `(,a . ,d) n))))
(define cdro
  (lambda (p d)
    (fresh (a)
      (== (cons a d) p))))
(define caro
  (lambda (p a)
    (fresh (d)
      (== (cons a d) p))))

;(include "numbers.scm")

(define myappendo (lambda (l s out)
    (conde
      ( (project (l s)
          (lambda (st)
            (printf "~a ~a\n" l s)
            (succeed) ))
        (== '() l)
        (== s out) )
      ((fresh (a d res)
         (== `(,a . ,d) l)
         (== `(,a . ,res) out)
         (myappendo d s res))) )))

(run 2 (q r)
  (myappendo q '() r ))
