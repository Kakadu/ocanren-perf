(define (list-display lis)
          (cond ((null? lis)
                 #f)
                (else
                 (display (car lis))
                 (newline)
                 (list-display (cdr lis)))))

(define not-in-envo (lambda (x env)
   (conde
     ((fresh (y v rest)
        (== env `((,y . ,v) . ,rest) )
        (=/= y x)
        (not-in-envo x rest)
         ))
     ((== '() env)) )))


(define proper-listo
 (lambda (exp env rs)
   (conde
     ((== '() exp) (== '() rs))
     ((fresh (e d t-e t-d)
        (== exp `(,e . ,d) )
        (== rs `(,t-e . ,t-d) )
        (eval-expo e env `(val_ ,t-e) )
        (proper-listo d env t-d) )) )))

;(expand/step #'(lambda (exp env r)
;    (conde
;      ((fresh (t)
;         (== exp `(seq ((symb 'quote) ,t)) )
;         (== r `(val_ ,t))
;         (not-in-envo 'quote env)
;           ))
;      ((fresh (es rs)
;         (== exp `(seq ((symb 'list) . ,es)) )
;         (== r `(val_ (seq ,rs)) )
;         (not-in-envo 'list env)
;         (proper-listo es env rs)
;           )
;       )
;      )
;                 )
;
;            )


(define eval-expo
  (lambda (exp env r)
    (conde
      ((fresh (t)
         (== exp `(seq ((symb 'quote) ,t)) )
         (== r `(val_ ,t))
         (not-in-envo 'quote env)
           ))
      ((fresh (es rs)
         (== exp `(seq ((symb 'list) . ,es)) )
         (== r `(val_ (seq ,rs)) )
         (not-in-envo 'list env)
         (proper-listo es env rs)
           ))

      ((fresh (s)
         (== exp `(symb ,s))
         (lookupo s env r) ))
      ((fresh (rator rand x body env^ a)
         (== exp `(seq (,rator ,rand)) )
         (eval-expo rand env a)
         (eval-expo rator env `(closure ,x ,body ,env^))
         (eval-expo body `((,x . ,a) . ,env^) r) ))
      ((fresh (x body)
         (== exp `(seq ( (symb 'lambda)
                         (seq ((symb ,x)))
                         ,body) ) )
         (not-in-envo 'lambda env)
         (== r `(closure ,x ,body ,env) ) ))
         )) )

(define lookupo
  (lambda (x env t)
    (fresh (rest y v)
      (== `((,y . ,v) . ,rest) env)
      (conde
        ((== y x) (== v t))
        ((=/= y x) (lookupo x rest t))))))

#|
(test-check "4 thrines"
  (run 4 (x)
    (fresh (p q r)
      (=/= p q)
      (=/= q r)
      (=/= r p)
      (eval-expo p '() q)
      (eval-expo q '() r)
      (eval-expo r '() p)
      (== `(,p ,q ,r) x)))
  '(((''((lambda (_.0)
           (list 'quote (list 'quote (list _.0 (list 'quote _.0)))))
         '(lambda (_.0)
            (list 'quote (list 'quote (list _.0 (list 'quote _.0))))))
      '((lambda (_.0)
          (list 'quote (list 'quote (list _.0 (list 'quote _.0)))))
        '(lambda (_.0)
           (list 'quote (list 'quote (list _.0 (list 'quote _.0))))))
      ((lambda (_.0)
         (list 'quote (list 'quote (list _.0 (list 'quote _.0)))))
       '(lambda (_.0)
          (list 'quote (list 'quote (list _.0 (list 'quote _.0)))))))
     (=/= ((_.0 . closure)) ((_.0 . list)) ((_.0 . quote)))
     (sym _.0))
    ((''((lambda (_.0)
           (list
             'quote
             (list
               'quote
               (list ((lambda (_.1) _.0) '_.2) (list 'quote _.0)))))
         '(lambda (_.0)
            (list
              'quote
              (list
                'quote
                (list ((lambda (_.1) _.0) '_.2) (list 'quote _.0))))))
      '((lambda (_.0)
          (list
            'quote
            (list
              'quote
              (list ((lambda (_.1) _.0) '_.2) (list 'quote _.0)))))
        '(lambda (_.0)
           (list
             'quote
             (list
               'quote
               (list ((lambda (_.1) _.0) '_.2) (list 'quote _.0))))))
      ((lambda (_.0)
         (list
           'quote
           (list
             'quote
             (list ((lambda (_.1) _.0) '_.2) (list 'quote _.0)))))
       '(lambda (_.0)
          (list
            'quote
            (list
              'quote
              (list ((lambda (_.1) _.0) '_.2) (list 'quote _.0)))))))
     (=/= ((_.0 . closure)) ((_.0 . lambda)) ((_.0 . list))
          ((_.0 . quote)) ((_.1 . _.0)) ((_.1 . closure)))
     (no-closure _.2)
     (sym _.0 _.1))
    (('(list
         '(lambda (_.0)
            (list
              'quote
              (list 'list _.0 (list 'quote (list 'quote _.0)))))
         '''(lambda (_.0)
              (list
                'quote
                (list 'list _.0 (list 'quote (list 'quote _.0))))))
      (list
        '(lambda (_.0)
           (list
             'quote
             (list 'list _.0 (list 'quote (list 'quote _.0)))))
        '''(lambda (_.0)
             (list
               'quote
               (list 'list _.0 (list 'quote (list 'quote _.0))))))
      ((lambda (_.0)
         (list
           'quote
           (list 'list _.0 (list 'quote (list 'quote _.0)))))
       ''(lambda (_.0)
           (list
             'quote
             (list 'list _.0 (list 'quote (list 'quote _.0)))))))
     (=/= ((_.0 . closure)) ((_.0 . list)) ((_.0 . quote)))
     (sym _.0))
    ((''((lambda (_.0)
           (list
             ((lambda (_.1) 'quote) '_.2)
             (list 'quote (list _.0 (list 'quote _.0)))))
         '(lambda (_.0)
            (list
              ((lambda (_.1) 'quote) '_.2)
              (list 'quote (list _.0 (list 'quote _.0))))))
      '((lambda (_.0)
          (list
            ((lambda (_.1) 'quote) '_.2)
            (list 'quote (list _.0 (list 'quote _.0)))))
        '(lambda (_.0)
           (list
             ((lambda (_.1) 'quote) '_.2)
             (list 'quote (list _.0 (list 'quote _.0))))))
      ((lambda (_.0)
         (list
           ((lambda (_.1) 'quote) '_.2)
           (list 'quote (list _.0 (list 'quote _.0)))))
       '(lambda (_.0)
          (list
            ((lambda (_.1) 'quote) '_.2)
            (list 'quote (list _.0 (list 'quote _.0)))))))
     (=/= ((_.0 . closure)) ((_.0 . lambda)) ((_.0 . list))
          ((_.0 . quote)) ((_.1 . closure)) ((_.1 . quote)))
     (no-closure _.2)
     (sym _.0 _.1))))

(test-check "thrines proof"
  (let ([p/q/r (caar (run 1 (x)
                       (fresh (p q r)
                         (=/= p q)
                         (=/= q r)
                         (=/= r p)
                         (eval-expo p '() q)
                         (eval-expo q '() r)
                         (eval-expo r '() p)
                         (== `(,p ,q ,r) x))))])
    (let ([p (car p/q/r)]
          [q (cadr p/q/r)]
          [r (caddr p/q/r)])
      (and
        (equal? (eval p) q)
        (equal? (eval q) r)
        (equal? (eval r) p)
        (not (equal? p q))
        (not (equal? q r))
        (not (equal? r p)))))
  #t)

|#
