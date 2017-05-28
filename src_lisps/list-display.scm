(define (list-display lis)
          (cond ((null? lis)
                 #f)
                (else
                 (display (car lis))
                 (newline)
                 (list-display (cdr lis)))))

(define logged_unif_counter 0)

(define ===
 (lambda (x y)
   (lambda (s)
     (let ( ;(x2 (walk x s))
            ; (y2 (walk y s))
          )
        (set! logged_unif_counter (+ 1 logged_unif_counter))
        (printf "~a: unify '~a' and '~a'\n" logged_unif_counter x y)
        ((== x y) s) ))))
