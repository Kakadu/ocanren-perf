#lang racket
(require math/statistics
         benchmark
         pretty-format)
(require "./mk.rkt")

(require (prefix-in vanilla: "wrap-vanilla.rkt")
         (prefix-in ocanren: "wrap-ocanren.rkt"))

(define repeat
  (let ([data (getenv "REPEAT")])
    (match data
      [#f 40]
      [s (string->number data)])))

(pretty-printf "~a repetitions\n" repeat)

(define results
  (run-benchmarks (list 'old-100quines 'new-100quines)
                  '()
                  (lambda (op)
                    (match op
                      ['old-100quines (run 100 (q) (vanilla:eval-expo q '() q))]
                      ['new-100quines (run 100 (q) (ocanren:eval-expo q '() `(val_ ,q)))]

                      ['sleepHalf (sleep 0.5)]))
                  ; don't extract time, instead time (run ...)
                  #:extract-time 'delta-time
                  #:num-trials repeat ; TODO: 40 is better
                  #:results-file "quines_bench_racket.sexp"))

(for ([i results])
  (pretty-printf "~a: ~a (mean for ~a repeatitions)\n"
                 (benchmark-result-name i)
                 (mean (benchmark-result-trial-times i))
                 repeat))
