(include "../faster-miniKanren/mk-vicare.scm")
(include "../faster-miniKanren/mk.scm")
(include "list-display.scm")

(define appendo (lambda (xs ys xys)
  (conde
    ((== xs '()) (== ys xys))
    ((fresh (h tl temp)
      (== xs `(,h . ,tl))
      (== xys `(,h . ,temp))
      (appendo tl ys temp)
      ))
  )))

(define (reverso1 xy yx)
  (conde
    [(== xy '()) (== yx '())]
    [(fresh (h tl tmp)
      (== xy `(,h . ,tl))
      (appendo tmp `(,h) yx)
      (reverso1 tl tmp))]))

(define (reverso2 xy yx)
  (conde
    [(== xy '()) (== yx '())]
    [(fresh (h tl tmp)
      (== xy `(,h . ,tl))
      (reverso2 tl tmp)
      (appendo tmp `(,h) yx))]))

;(list-display
;  (run* (q)
;    (appendo '(1 2) '(3 4) q)))

(list-display
  (run 1 (q)
    (reverso1 '(1 2 3 4) q)))

; hangs
;(list-display
;  (run* (q)
;    (reverso1 '(1 2 3 4) q)))
(list-display
  (run* (q)
    (reverso1 q '(1 2 3 4))))

(list-display
  (run* (q)
    (reverso2 '(1 2 3 4) q)))
(list-display
  (run 1 (q)
    (reverso2 q '(1 2 3 4))))
; hangs
;(list-display
;  (run* (q)
;    (reverso2 q '(1 2 3 4))))

(define (reverso3 xy yx)
  (conde
    [(== xy '()) (== yx '())]
    [(project (xy)
        (cond
          ( (var? xy)
            (fresh (h tl tmp)
              (== xy `(,h . ,tl))
              (appendo tmp `(,h) yx)
              (reverso3 tl tmp)))
          (else
            (fresh (h tl tmp)
              (== xy `(,h . ,tl))
              (reverso3 tl tmp)
              (appendo tmp `(,h) yx))))) ]))

;(parameterize ([print-gensym 'pretty/suffix])
;(pretty-print (expand
;  '(conde [(== xy '()) (== yx '())])
;)))

(define (debugrel msg)
  (lambda (st)
    (begin
      (display msg)
      (display "\n")
      (succeed st))))

(list-display
  (run* (q)
    (debugrel "hacky reverso forward")
    (reverso3  '(1 2 3) q)))

(list-display
  (run* (q)
    (debugrel "hacky reverso backwards")
    (reverso3  q '(1 2 3) )))
