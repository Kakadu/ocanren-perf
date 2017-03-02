#lang racket
(require racket/include)

(require "../faster-miniKanren/mk.rkt")

;(include "../webyrd-quines/q.scm")
(include "q.scm")

; quines
(define answers
  (run 5 (p) (eval-expo p '() `(val_ ,p)) ) )

(map println answers)


;(list-display
;  (run 13 (p) (fresh (r) (eval-expo p '() r ) )) )

;(define cinqs
;  (lambda (q)
;    (conde
;      ((== q 5))
;      ((cinqs q)) )
;    ))
;
;(define sixs
;  (lambda (q)
;    (conde
;      ((== q 6))
;      ((sixs q)) )
;    ))
;
;(define septs
;  (lambda (q)
;    (conde
;      ((== q 7))
;      ((septs q)) )
;    ))
;
;(define foo (lambda (q)
;  (conde
;    ((cinqs q))
;    ((sixs  q))
;    ((septs q))
;      ) ))
;
;(run 9 (q) (foo q) )
;; '(5 6 5 7 5 6 5 7 5)
