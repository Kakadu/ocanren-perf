#lang racket

(require pretty-format)

(require "mk.rkt")

(define unif-counter 0)
(define report_counters (lambda () (pretty-printf "~a unifications.\n" unif-counter)))

(define ===
  (lambda (x y)
    (lambda (s)
      (begin
        (set! unif-counter (add1 unif-counter))
        ((== x y) s)))))

(define =//= (lambda (x y) (lambda (s) ((=/= x y) s))))
(define (takeMK n f)
  (take n f))
(include "src_lisps/list-display.scm")
(include "src_lisps/q.scm")
(include "src_lisps/numbers.scm")

(define myrunN
  (lambda (n rel)
    (let* ([st empty-state]
           [scope (subst-scope (state-S st))]
           [q (var scope)])
      (map (lambda (st0) ((reify q) (state-with-scope st0 nonlocal-scope)))
           ;
           (take n ((rel q) st))))))

; TODO: Implement a simplifier for weird quines

;(myrunN 2 (lambda (q) (eval-expo q '() `(val_ ,q))))

(define wrap
  (lambda (n f)
    (begin
      (list-display (myrunN n f))
      (report_counters))))

(command-line #:once-each [("--app1")
                           ""
                           (begin
                             (run 1 (q) (eval-expo '('asdf) '() q))
                             (report_counters))]
              ; quines
              [("--firstQ") n "" (wrap (string->number n) (lambda (q) (eval-expo q '() `(val_ ,q))))]
              ; Oleg numbers
              [("--mul1x1") "" (wrap 1 (lambda (q) (*o (build-num 1) (build-num 1) q)))]
              [("--mul1x2") "" (wrap 1 (lambda (q) (*o (build-num 1) (build-num 2) q)))]
              [("--mul2x2") "" (wrap 1 (lambda (q) (*o (build-num 2) (build-num 2) q)))]
              [("--mul2x3") "" (wrap 1 (lambda (q) (*o (build-num 2) (build-num 3) q)))]
              [("--mul3x2") "" (wrap 1 (lambda (q) (*o (build-num 3) (build-num 2) q)))]
              [("--mul3x3") "" (wrap 1 (lambda (q) (*o (build-num 3) (build-num 3) q)))]
              [("--mul4x4") "" (wrap 1 (lambda (q) (*o (build-num 4) (build-num 4) q)))]
              [("--mul3x5") "" (wrap 1 (lambda (q) (*o (build-num 3) (build-num 5) q)))]
              [("--mul5x3") "" (wrap 1 (lambda (q) (*o (build-num 5) (build-num 3) q)))]
              [("--mul5x4") "" (wrap 1 1 (lambda (q) (*o (build-num 5) (build-num 4) q)))]
              [("--mul5x5") "" (wrap 1 1 (lambda (q) (*o (build-num 5) (build-num 5) q)))]
              [("--mul5x5-all") "" (wrap 1 1 (lambda (q) (*o (build-num 5) (build-num 5) q)))]
              [("--mul5x6") "" (wrap 1 (myrunN 1 (lambda (q) (*o (build-num 5) (build-num 6) q))))]
              [("--mul7x7") "" (wrap 1 (lambda (q) (*o (build-num 7) (build-num 7) q)))]
              [("--mul127x127") "" (wrap 1 (lambda (q) (*o (build-num 127) (build-num 127) q)))]
              [("--mul255x255")
               ""
               (wrap 1
                     (lambda (q)
                       *o
                       (build-num 255)
                       (build-num 255)
                       q))]
              [("--exp2x3") "" (wrap 1 (lambda (q) (expo (build-num 2) (build-num 3) q)))]
              [("--exp3x2") "" (wrap 1 (lambda (q) (expo (build-num 3) (build-num 2) q)))]
              [("--exp3x5") "" (wrap 1 (lambda (q) (expo (build-num 3) (build-num 5) q)))]
              [("--exp7x2") "" (wrap 1 (lambda (q) (expo (build-num 7) (build-num 2) q)))]
              [("--logo8base2") "" (wrap 1 (lambda (q) (expo (build-num 2) q (build-num 8))))]
              [("--logo243base3") "" (wrap 1 (lambda (q) (expo (build-num 3) q (build-num 243))))]
              ; other
              )
