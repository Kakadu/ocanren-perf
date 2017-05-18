(include "../faster-miniKanren/mk-vicare.scm")
(include "../faster-miniKanren/mk.scm")
(include "list-display.scm")


;(define evalo (lambda (m)
;  (begin (printf " applying evalo to m\n")
;  (fresh (f2)
;      (conde
;        ((fresh (x)
;            (=== f2 1)
;        ))
;        ;((lambdag@ (st)
;        ;    (let ( (x (var 'x (subst-scope (state-S st)))) )
;        ;     (printf "create inc in fresh ==== ~a\n" (list 'x))
;        ;     (inc (begin
;        ;           (printf "inc in fresh forced: ~a \n" (list 'x))
;        ;           ((=== f2 1) st) )))))
;        ((fresh (p)
;            (=== f2 2)
;        ))
;
;        ;((lambdag@ (st)
;        ;    (let ( (p (var 'p (subst-scope (state-S st)))) )
;        ;     (printf "create inc in fresh ==== ~a\n" (list 'p))
;        ;     (inc (begin
;        ;           (printf "inc in fresh forced: ~a \n" (list 'p))
;        ;           ((=== f2 2) st) )))))
;      )
;      (evalo 4)
;  )
;)))

;(define evalo (lambda (m)
;  (begin (printf " applying evalo to m\n")
;  (fresh (f2)
;      ;(conde
;      (lambdag@ (st)
;         (begin
;           (printf " created inc in conde\n")
;           (inc
;            (let ((st (state-with-scope st (new-scope))))
;               (printf " force a conde\n")
;               (mplus*
;                    (let ( (x (var 'x (subst-scope (state-S st)))) )
;                          (printf "create inc in fresh ==== ~a\n" (list 'x))
;                          (inc (begin
;                                (printf "inc in fresh forced: ~a \n" (list 'x))
;                                ((=== f2 1) st) )))
;
;                    (let ( (p (var 'p (subst-scope (state-S st)))) )
;                     (printf "create inc in fresh ==== ~a\n" (list 'p))
;                     (inc (begin
;                           (printf "inc in fresh forced: ~a \n" (list 'p))
;                           ((=== f2 2) st) ))))
;                      ))))
;      (evalo 4)
;  )
;)))

;(define evalo (lambda (m)
;  (begin (printf " applying evalo to m\n")
;  (fresh (f2)
;      (lambdag@ (st)
;        (begin
;          (printf " created inc in conde\n")
;          (inc
;            (let ((st (state-with-scope st (new-scope))))
;                (printf " force a conde\n")
;                (mplus
;                  (let ( (x (var 'x (subst-scope (state-S st)))) )
;                          (printf "create inc in fresh ==== ~a\n" (list 'x))
;                          (inc (begin
;                                (printf "inc in fresh forced: ~a \n" (list 'x))
;                                ((=== f2 1) st) )))
;                  (inc (begin
;                          (printf " force inc from mplus*\n")
;                          (let ( (p (var 'p (subst-scope (state-S st)))) )
;                             (printf "create inc in fresh ==== ~a\n" (list 'p))
;                             (inc (begin
;                                   (printf "inc in fresh forced: ~a \n" (list 'p))
;                                   ((=== f2 2) st) ))))
;                  )
;                )
;              )
;            ) ))
;      (evalo 4)
;  )
;)))

(define evalo (lambda (m)
  (begin (printf " applying evalo to m\n")
  (lambdag@ (st)
    (let ((scope (subst-scope (state-S st))))
      (let ((f2 (var 'f2 scope)) )
        (printf "create inc in fresh ==== ~a\n" (list 'f2 ) )
        (inc (begin
            (printf "inc in fresh forced: ~a \n" (list 'f2 ))
            (bind
              (begin (printf " created inc in conde\n")
                (inc
                  (let ((st (state-with-scope st (new-scope))))
                    (printf " force a conde\n")
                    (mplus
                      (let ( (x (var 'x (subst-scope (state-S st)))) )
                               (printf "create inc in fresh ==== ~a\n" (list 'x))
                               (inc (begin
                                     (printf "inc in fresh forced: ~a \n" (list 'x))
                                     ((=== f2 1) st) )))
                      (inc (begin
                        (printf " force inc from mplus*\n")
                        (let ( (p (var 'p (subst-scope (state-S st)))) )
                            (printf "create inc in fresh ==== ~a\n" (list 'p))
                            (inc (begin
                                  (printf "inc in fresh forced: ~a \n" (list 'p))
                                  ((=== f2 2) st) ))))
                      )
                    )
                  )
                )
              )
              (evalo 4) )
            ))
        )))
)))


(list-display
  (myrun1 1 (q)
    (evalo q)
  )
)
