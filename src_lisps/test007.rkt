#lang racket
(require racket/include)

(require "../faster-miniKanren/mk.rkt")
;(require macro-debugger/syntax-browser)
;(require macro-debugger/stepper)
;(include "../webyrd-quines/q.scm")
(include "q.scm")

; quines
(define answers
  (run 22 (p) (eval-expo p '() `(val_ ,p)) ) )

(map println answers)

;(expand/step #'(run 1 (p) (fresh (r) (eval-expo p '() r ) )) )


;(list-display
;  (run 22 (p) (fresh (r) (eval-expo p '() r ) )) )

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
