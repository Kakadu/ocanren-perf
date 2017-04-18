(include "../simple-miniKanren/mk.scm")

(define ===
  (lambda (x y)
    (lambda (s)
      (let ((x2 (walk x s))
            (y2 (walk y s)))
        (printf "unify '~a' and '~a'\n" x2 y2)
        ((== x y) s) ))))

(define myappendo (lambda (a b ab)
    (conde
      (
        ;(project (a b ab) (lambda (st)
        ;  (printf "appendo simple (a,b,ab): ~a ~a ~a\n" a b ab)
        ;  (succeed st) ))
        (=== a '())
        (=== b ab) )
      ((fresh (h tl res)
          ;(project (a b ab) (lambda (st)
          ;  (printf "appendo complex (a,b,ab): ~a ~a ~a\n" a b ab)
          ;  (succeed st) ))
          (=== a  `(,h . ,tl))
          (=== ab `(,h . ,res))
          (myappendo tl b res))) )))

; without display REPL prints the result but compiled code doesn't
(map (lambda (x) (printf "~a\n" x))
  (run 1 (q) (myappendo q '(3 4) '(1 2 3 4) ))
    )
