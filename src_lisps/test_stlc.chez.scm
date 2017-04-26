(include "../faster-miniKanren/mk-vicare.scm")
(include "../faster-miniKanren/mk.scm")
(include "list-display.scm")


(define substo (lambda (l x a l2)
  (conde
    ((fresh (y)
        (== l `(v ,y))
        (== y x)
        (== l2 a) ))
    ((fresh (m n m2 n2)
        (== l  `(app ,m  ,n))
        (== l2 `(app ,m2 ,n2))
        (substo m x a m2)
        (substo n x a n2) ))
    ((fresh (v b)
        (== l `(abs ,v ,b))
        (conde
          ((== x v) (== l2 l))
          ((fresh (b2)
              (== l2 `(abs ,v ,b2))
              (substo b x a b2) ))
        )
    ))
  )
))

(define evalo (lambda (m n)
  (conde
    ((fresh (x)
        (== m `(v ,x))
        (== n m)))
    ((fresh (x l)
        (== m `(abs ,x ,l))
        (== n m) ))
    ((fresh (f a f2 a2)
        (== m `(app ,f ,a))
        (conde
          ((fresh (x l l2)
              (== f2 `(abs ,x ,l))
              (substo l x a2 l2)
              (evalo l2 n) ))
          ((fresh (p q)
              (== f2 `(app  ,p  ,q))
              (== n  `(app ,f2 ,a2)) ))
          ((fresh (x)
              (== f2 `(v ,x))
              (== n  `(app ,f2 ,a2))))
        )))

  )
))

(list-display
  (run 2 (q r s)
    (evalo `(app ,q ,r) s)
    (evalo `(app ,r ,s) q)
    (evalo `(app ,s ,q) r)
  )
)
