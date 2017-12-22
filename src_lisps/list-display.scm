(define (list-display lis)
          (cond ((null? lis)
                 #f)
                (else
                 (display (car lis))
                 (newline)
                 (list-display (cdr lis)))))

(define with_real_time
  (lambda (foo)
    (let ((tbegin (real-time) ))
      (foo)
      (let ((tend (real-time) ))
        (printf "Whole time is ~a ms\n" (/ (- tend tbegin) 1000.0))
))))

; Use this, for example, like this
;; (if (not (getenv "DONT_RUN_CHEZ"))
;;     (begin
;;       (with_real_time (lambda () (list-display (do_measure)) ))
;;       ))



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
