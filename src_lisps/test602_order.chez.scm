(include "../faster-miniKanren/mk-vicare.scm")
(include "../faster-miniKanren/mk.scm")
(include "list-display.scm")


;(define evalo (lambda (m n)
;  (conde
;    ((fresh (f f2)
;        (conde
;          ;((fresh (x)
;          ;    (=== f2 1)
;          ;))
;          ((lambdag@ (st)
;              (let ( (x (var 'x (subst-scope (state-S st)))) )
;               (printf "create inc in fresh ==== ~a\n" (list 'x))
;               (inc (begin
;                     (printf "inc in fresh forced: ~a \n" (list 'x))
;                     ((=== f2 1) st) )))))
;          ;((fresh (p)
;          ;    (=== f2 2)
;          ;))
;          ((lambdag@ (st)
;              (let ( (p (var 'p (subst-scope (state-S st)))) )
;               (printf "create inc in fresh ==== ~a\n" (list 'p))
;               (inc (begin
;                     (printf "inc in fresh forced: ~a \n" (list 'p))
;                     ((=== f2 2) st) )))))
;        )
;        (evalo f f2)
;    ))
;  )
;))


(define evalo (lambda (m n)
    (fresh (f f2)
        ;(conde
        (lambdag@ (st)
           (begin
             (printf " created inc in conde\n")
             (inc
              (let ((st (state-with-scope st (new-scope))))
                 (printf " force a conde\n")
                 (mplus*
                   ((lambdag@ (st)
                       (let ( (x (var 'x (subst-scope (state-S st)))) )
                        (printf "create inc in fresh ==== ~a\n" (list 'x))
                        (inc (begin
                              (printf "inc in fresh forced: ~a \n" (list 'x))
                              ((=== f2 1) st) )))) st)
                    ((lambdag@ (st)
                        (let ( (p (var 'p (subst-scope (state-S st)))) )
                         (printf "create inc in fresh ==== ~a\n" (list 'p))
                         (inc (begin
                               (printf "inc in fresh forced: ~a \n" (list 'p))
                               ((=== f2 2) st) )))) st)
                ))))

        )
        (evalo f f2)
    )
))

;(define evalo (lambda (m n)
;    (fresh (f f2)
;        ;(conde
;        (lambdag@ (st)
;           (begin
;             (printf " created inc in conde\n")
;             (inc
;              (let ((st (state-with-scope st (new-scope))))
;                 (printf " force a conde\n")
;                 (mplus*
;                   ((lambdag@ (st)
;                       (let ( (x (var 'x (subst-scope (state-S st)))) )
;                        (printf "create inc in fresh ==== ~a\n" (list 'x))
;                        (inc (begin
;                              (printf "inc in fresh forced: ~a \n" (list 'x))
;                              ((=== f2 1) st) )))) st)
;                    ((lambdag@ (st)
;                        (let ( (p (var 'p (subst-scope (state-S st)))) )
;                         (printf "create inc in fresh ==== ~a\n" (list 'p))
;                         (inc (begin
;                               (printf "inc in fresh forced: ~a \n" (list 'p))
;                               ((=== f2 2) st) )))) st)
;                ))))
;
;        )
;        (evalo f f2)
;    )
;))

(list-display
  (myrun2 1 (q r)
    (evalo q r)
  )
)
