(define (list-display lis)
          (cond ((null? lis)
                 #f)
                (else
                 (display (car lis))
                 (newline)
                 (list-display (cdr lis)))))

(define logged_unif_counter 0)

(define === (lambda (x y) (lambda (s)
  (log_unif)
  (printf "~a: unify '~a' and '~a'\n" logged_unif_counter x y)
  ((== x y) s)
)))

(define =//= (lambda (x y) (lambda (s)
  (log_diseq)
  (printf "~a: (=/=) '~a' and '~a'\n" logged_diseq_counter x y)
  ((=/= x y) s)
)))
