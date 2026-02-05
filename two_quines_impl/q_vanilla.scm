; Old quines, like in faster-miniKanren



(define lookupo
  (lambda (x env t)
    (fresh (rest y v)
      (=== `((,y . ,v) . ,rest) env)
      (conde
        ((=== y x) (=== v t))
        ((=//= y x) (lookupo x rest t))))))


(define not-in-envo (lambda (x env)
    (conde
      ((fresh (y v rest)
         (=== env `((,y . ,v) . ,rest))
         (=/= y x)
         (not-in-envo x rest)
      ))
      ((== '() env)))))


(define proper-listo
  (lambda (exp env val)
    (conde
      ((=== '() exp)
       (=== '() val)
      )
      ((fresh (a d t-a t-d)
         (=== exp `(,a . ,d))
         (=== val `(,t-a . ,t-d))
         (eval-expo a env t-a)
         (proper-listo d env t-d))))))

(define eval-expo
  (lambda (exp env val)
    (conde
      ((fresh (v)
         (=== `(quote ,v) exp)
         (=== v val)
         (not-in-envo 'quote env)
         (absento 'closure v)
      ))
      ((fresh (a*)
         (=== `(list . ,a*) exp)
         (not-in-envo 'list env)
         (absento 'closure a*)
         (proper-listo a* env val)))
      ((fresh (s)
        (symbolo exp)
        (lookupo exp env val)
      ))
      ((fresh (rator rand x body env^ a)
         (=== exp `(,rator ,rand) )
         (eval-expo rand env a)
         (eval-expo rator env `(closure ,x ,body ,env^))
         (eval-expo body `((,x . ,a) . ,env^) val)
      ))

      ((fresh (x body)
         (=== exp `(lambda (,x) ,body))
         (symbolo x)
         (not-in-envo 'lambda env)
         (=== val `(closure ,x ,body ,env) )))
      )))
