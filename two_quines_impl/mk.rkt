#lang racket/base

(provide run run*
    ;defrel
    == =/=
    fresh
    conde
    symbolo numbero stringo
    absento
    ;project
    reify
    empty-state
    var?
    var-idx
    walk*
    ; A few primitives to be able to run macro expanded relations
    state-with-scope
    new-scope
    subst-scope
    state-S
    mplus
    bind
    ; two macro to debug macro
    bind*
    mplus*
    var
    suspend
    always-wrap-reified?
    ; for custom run
    nonlocal-scope
    ;takeMK
    )

(require "../faster-miniKanren/private-unstable.rkt")


; (define (mplus stream f)
;   (case-inf stream
;     (() (begin
;             (display "  mplus 1st branch\n")
;             f))
;     ((f^) (begin
;             (display "  mplus 2nd branch\n")
;             (lambda () (begin
;               (display "HERE\n")
;               (mplus (f) f^)))))
;     ((c) (begin
;             (display "  mplus 3rd branch\n")
;             (cons c f)))
;     ((c f^) (begin
;               (display "  mplus 4th branch\n")
;               (cons c (lambda () (mplus (f) f^)))))))

; ; SearchStream, Goal -> SearchStream
; (define (bind stream g)
;   (case-inf stream
;     (() (begin
;             (display "  bind 1st branch\n")
;             #f))
;     ((f) (begin
;             (display "  bind 2nd branch\n")
;             (lambda () (bind (f) g))))
;     ((c) (begin
;             (display "  bind 3rd branch\n")
;             (g c)))
;     ((c f) (begin
;               (display "  bind 4th branch\n")
;               (mplus (g c) (lambda () (bind (f) g)))))))

; we create 10 variables to start OCanren and faster-miniKanren counters from the same number
; (begin
;   (let (
;     (v1 (var 1))
;     (v2 (var 1))
;     (v3 (var 1))
;     (v4 (var 1))
;     (v5 (var 1))
;     (v6 (var 1))
;     (v7 (var 1))
;     (v8 (var 1))
;     (v9 (var 1))
;     (v0 (var 1)) )

;     (values))
;   )
