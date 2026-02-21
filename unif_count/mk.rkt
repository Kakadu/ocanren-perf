#lang racket/base

(provide run
         run*
         ;defrel
         ==
         =/=
         fresh
         conde
         symbolo
         numbero
         stringo
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
         take
         ;
         )

(require "faster-miniKanren/private-unstable.rkt")
