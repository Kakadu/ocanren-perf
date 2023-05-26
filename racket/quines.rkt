#lang racket
(require benchmark plot/pict racket/vector racket/list)

(require "../faster-orig/mk.rkt")
;(define === (lambda (a b) (== a b)))

(require "../src_lisps/q.rkt")

; list/vector sizes
; (define sizes (list 50000 100000))
; (define lists (map (lambda (i) (range i)) sizes))
; (define vectors (map list->vector lists))

(define bench_quines (lambda (n)
  (run n (p)
    (eval-expo p '() `(val_ ,p)))
))

; debug warmup 
; (pretty-print (bench 10))

(if (not (getenv "BENCH_MODE"))
  (begin
    (pretty-print (bench_quines 200))
    (exit)
  )
  (pretty-print
    (run-benchmarks
     ; operations (whats)
     (list 'quines)
     ; list of options (hows)
     (list
      ; sizes (and their indices) in the sizes list
      '(200)
      ; implementations of operations
    ;  '(1)
    )
     ; to run each benchmark
     (lambda (op size)
         (bench_quines size))
     ; don't extract time, instead time (run ...)
     #:extract-time 'delta-time
     #:num-trials 5 ; TODO: 40 is better
     #:results-file "quines.sexp"
    ))
)


; (define results
;     (run-benchmarks
;      (list 'quines)
;      (list
;       '(200)
;     )
;      (lambda (op size)
;          (bench_quines size))
;      #:extract-time 'delta-time
;      #:num-trials 5 ; TODO: 40 is better
;      #:results-file "quines.sexp"
;     ))


; (pretty-print results)
; (parameterize ([plot-x-ticks no-ticks])
;     (plot-pict
;      #:title "vectors vs lists"
;      #:x-label #f
;      #:y-label "normalized time"
;      (render-benchmark-alts
;       ; default options
;       (list (cons 0 50000) 'list)
;       results
;       ; format options so we can omit the index in the size list
;       #:format-opts (lambda (opts)
;                       (let ([index/size (car opts)]
;                             [impl (cadr opts)])
;                         (format "size: ~a, ~a" (cdr index/size) impl))))))