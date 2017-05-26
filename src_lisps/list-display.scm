(define (list-display lis)
          (cond ((null? lis)
                 #f)
                (else
                 (display (car lis))
                 (newline)
                 (list-display (cdr lis)))))

(define ===
 (lambda (x y)
   (lambda (s)
     (let ( ;(x2 (walk x s))
            ; (y2 (walk y s))
          )
       ;(printf "unify '~a' and '~a'\n" x y)
       ((== x y) s) ))))
