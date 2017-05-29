(include "../faster-miniKanren/mk-vicare.scm")
(include "../faster-miniKanren/mk.scm")

(include "q.scm")

(list-display
  (run 2 (exp)
    (conde
      ((fresh (t)
          (=== exp 0)
          (=== exp 1)
          ))
      ((fresh (es)
          (=== exp 2)))
      ((fresh (zzzzzzzzzs)
          (=== exp 3)))
          ))

)

;(printf "unif-counter = ~a\n" unif-counter)

;
;(list-display (run 2 (exp)
;  ; conde
;  (lambdag@ (st)
;    (let ((st (state-with-scope st (new-scope))))
;      (mplus
;        ; fresh
;        (
;           (begin
;              (let ((scope (subst-scope (state-S st))))
;                (let ( (t (var 't scope)) )
;                   (inc (begin
;                     (bind* ((=== exp 1 ) st) )))))))
;        (inc
;          (mplus
;            (
;                (let ((scope (subst-scope (state-S st))))
;                  (let ( (es (var 'es scope)) )
;                     (inc (begin
;                       (bind* ((=== exp 2) st) ))))))
;
;            (
;                (let ((scope (subst-scope (state-S st))))
;                  (let ( (zzzzzzzzzzs (var 'zzzzzzzzzs scope)) )
;                     (inc (begin
;                       (bind* ((=== exp 3) st) ))))))
;          )
;        ))
;      )
;    )
;))

;(list-display (run 2 (exp)
;  (lambdag@ (st)
;    (lambda ()
;     (let ((st (state-with-scope st (new-scope))))
;      (mplus
;        ; Bind was here
;        (let ((scope (subst-scope (state-S st))))
;          (let ( (t (var 't scope)) )
;             (inc
;               ((=== exp 1 ) st) )))
;
;        (inc
;          (begin (printf "herr\n")
;          (mplus
;            ; Bind was here
;            (let ((scope (subst-scope (state-S st))))
;              (let ( (es (var 'es scope)) )
;                 (inc
;                   ((=== exp 2) st) )))
;          (inc
;            ; Bind was here
;            (let ((scope (subst-scope (state-S st))))
;              (let ( (zzzzzzzzzzs (var 'zzzzzzzzzs scope)) )
;                 (inc
;                   ((=== exp 3) st) )))))))
;  ))))
;))


;(list-display (run 2 (exp)
;    (conde
;      ((lambdag@ (st)
;            (let ((scope (subst-scope (state-S st))))
;              (let ( (t (var 't scope)) )
;                 (inc
;                   ((=== exp 1 ) st) )))))
;
;      ((lambdag@ (st)
;            (let ((scope (subst-scope (state-S st))))
;              (let ( (es (var 'es scope)) )
;                 (inc
;                   ((=== exp 2) st) )))))
;
;      ((lambdag@ (st)
;
;            (let ((scope (subst-scope (state-S st))))
;              (let ( (zzzzzzzzzzs (var 'zzzzzzzzzs scope)) )
;                 (inc
;                   ((=== exp 3) st) )))))
;
;
;    )
;))

(report_counters)
