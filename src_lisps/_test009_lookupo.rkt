#lang racket
(require racket/include)

(require "../faster-miniKanren/mk.rkt")
(include "../faster-miniKanren/test-check.scm")


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

(include "numbers.scm")

(define myappendo (lambda (l s out)
    (conde
      ((== '() l) (== s out))
      ((fresh (a d res)
         (== `(,a . ,d) l)
         (== `(,a . ,res) out)
         (appendo d s res))) )))

(define mylookupo (lambda (x env t)
   (fresh (rest y v)
     (== `((,y . ,v) . ,rest) env)
     (conde
       ((== y x) (== v t))
       ((=/= y x) (mylookupo x rest t))))))

(run* (q r)
  (mylookupo q '((1 2) (3 4) (5 6) (7 8) (9 10) (11 12) (13 14) (15 16) (17 18) (19 20) ) r))
