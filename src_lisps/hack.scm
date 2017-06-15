(define filename "FILENAME")
;(define filename "test001_pow.chez.scm")

;(load filename)

; warmum
(do_measure)

(define tbegin (real-time))

(define iter
  (lambda (n)
    (cond
      ( (= n 0) (void) )
      ( else
          (let ( (ans (do_measure)) )
              ;(printf "~a\n" ans)
              (iter (- n 1))
                ))
      )
  ))

(define n 5)
(iter n)
(define tend (real-time))
(define time_in_ms (/ (- tend tbegin) (* n 1000.0)))
(printf "~a\n" time_in_ms)
