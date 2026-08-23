#lang racket

(require pretty-format)
(require "mk.rkt")

(define unif-counter 0)
(define ===
  (lambda (x y)
    (lambda (s)
      (begin
        (set! unif-counter (add1 unif-counter))
        (dlog "U ~a" unif-counter)
        ((== x y) s)))))
(define =//= (lambda (x y) (lambda (s) ((=/= x y) s))))

(include "src_lisps/q.scm")

(define (goal1 q) (fresh (t) (== q t)))
(define (goal2 q) (conde ((fresh (t) (== q t))) ((fresh (u) (== q u)))))
(define (goal3 q) (conde ((fresh (t) (== q t))) ((fresh (u) (== q u))) ((fresh (v) (== q v)))))
(define (goal4 q) (conde ((fresh (t) (== q t))) ((fresh (u v) (== q u) (== v u)))))
(define (goal5 q) (fresh (t) (conde ((== q t)) ((== q t)))))
(define (goal6 q) (conde ((fresh (t) (fresh (u) (== q t) (== u t)))) ((fresh (v) (== q v)))))

(define (member xs q)
  (conde
    ((fresh (h t) (== xs `(,h . ,t)) (== q h)))
    ((fresh (h t) (== xs `(,h . ,t)) (member t q)))))
(define (goal7 q) (member '(1 2 3) q))

; OC minex2 g1: unify a var against a nested template
(define (goal8 q) (fresh (t) (== q `(seq ((symb 'quote) ,t)))))
; OC minex2 g2: not-in-envo on a 3-element env (fresh env + unify, matches OC)
(define (goal9 q) (fresh (env) (== env '((a . 1) (b . 2) (c . 3))) (not-in-envo 'z env) (== q 'ok)))

; OC minex g10: element of xs not equal to 99 (diseq + branching + recursion, multi-answer)
(define (filter xs q)
  (conde
    ((fresh (h t) (== xs `(,h . ,t)) (=//= h 99) (== q h)))
    ((fresh (h t) (== xs `(,h . ,t)) (filter t q)))))
(define (goal10 q) (filter '(1 99 2 99 3) q))

; OC minex2 g3: lookupo over a 2-element env, all bindings (Gterm + diseq + recursion, multi-answer)
(define (goal11 q) (fresh (x) (lookupo x '((a . v1) (b . v2)) q)))

; OC minex g12: mutual recursion (ma<->mb) + branching, elements of xs (multi-answer)
(define (ma xs q) (conde ((fresh (h t) (== xs `(,h . ,t)) (== q h))) ((fresh (h t) (== xs `(,h . ,t)) (mb t q)))))
(define (mb xs q) (conde ((fresh (h t) (== xs `(,h . ,t)) (== q h))) ((fresh (h t) (== xs `(,h . ,t)) (ma t q)))))
(define (goal12 q) (ma '(1 2 3) q))

(define goals (vector goal1 goal2 goal3 goal4 goal5 goal6 goal7 goal8 goal9 goal10 goal11 goal12))

(command-line #:once-each [("-q") "" (void)]
               [("--dlog") file "" (set-dlog-port! (open-output-file file #:exists 'truncate))]
               [("-g") n ""
                (let ((ans (run 10 (q) ((vector-ref goals (sub1 (string->number n))) q))))
                  (pretty-printf "goal ~a answers=~a\n" n (length ans)))]
               )
