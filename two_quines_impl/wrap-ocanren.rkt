#lang racket

(require racket/vector
         racket/list)
(require pretty-format)
(provide eval-expo)
(require "./mk.rkt")

(define ===
  (lambda (x y)
    (lambda (s)
      ((== x y) s)
      ; ((==count x y) s)
      )))
(define =//= (lambda (x y) (lambda (s) ((=/= x y) s))))

(include "q_ocanren.scm")
