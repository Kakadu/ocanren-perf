(define (list-display lis)
          (cond ((null? lis)
                 #f)
                (else
                 (display (car lis))
                 (newline)
                 (list-display (cdr lis)))))


