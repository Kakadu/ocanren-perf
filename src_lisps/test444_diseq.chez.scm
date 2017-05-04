(include "../faster-miniKanren/mk-vicare.scm")
(include "../faster-miniKanren/mk.scm")

;(display
;  (run* (q)
;    (fresh (x y)
;      (== q `(,x ,y))
;      (=/= x y)
;      (=/= y x)
;    )))

; (((_.0 _.1) (=/= ((_.0 _.1)))))

(display
  (run* (q)
    (fresh (a b)
      (==  q `(,a ,b))
      (=/= q `(5  6 ))
      (== a 5)
      (== b 6)
    )))
