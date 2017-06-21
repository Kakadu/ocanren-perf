(define (list-display lis)
          (cond ((null? lis)
                 #f)
                (else
                 (display (car lis))
                 (newline)
                 (list-display (cdr lis)))))

(define logged_unif_counter 0)

; TODO: how to check variable is bound

(define === (lambda (x y) (lambda (s)
  ;(if log_unif (log_unif) (void) )
  ;(printf "~a: unify '~a' and '~a'" logged_unif_counter x y)

  ; gives a very weird text
  ;(printf "~a: unify '~a' and '~a'\n" logged_unif_counter
  ;    (reify-S x s)
  ;     y)
  (let* ( (ans ((== x y) s) ) )
    ;(if ans (printf "  +\n") (printf "   -\n"))
    ans
  )

)))

(define =//= (lambda (x y) (lambda (s)
  ;(if log_diseq (log_diseq) (void) )

  ;(printf "~a: (=/=) '~a' and '~a'" logged_diseq_counter x y)
  (let* ( (ans ((=/= x y) s) ) )
    ;(if ans (printf "  +\n") (printf "   -\n"))
    ans
  )
)))
