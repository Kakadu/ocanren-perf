(define (makelist n)
  (letrec ((helper (lambda (i acc)
        (if (<= i 0) acc (helper (- i 1) (cons i acc)))
      )))
    (helper n '())
  ))

(define (list-display lis)
          (cond ((null? lis)
                 #f)
                (else
                 (display (car lis))
                 (newline)
                 (list-display (cdr lis)))))

(define logged_unif_counter 0)

; TODO: how to check variable is bound
