
(define eval-expo
  (lambda (exp env val)
    (conde
      ((fresh (v)
         (== `(quote ,v) exp)
         (not-in-envo 'quote env)
         (absento 'closure v)
         (== v val)))
      ((fresh (a*)
         (== `(list . ,a*) exp)
         (not-in-envo 'list env)
         (absento 'closure a*)
         (proper-listo a* env val)))
      ((symbolo exp) (lookupo exp env val))
      ((fresh (rator rand x body env^ a)
         (== `(,rator ,rand) exp)
         (eval-expo rator env `(closure ,x ,body ,env^))
         (eval-expo rand env a)
         (eval-expo body `((,x . ,a) . ,env^) val)))
      ((fresh (x body)
         (== `(lambda (,x) ,body) exp)
         (symbolo x)
         (not-in-envo 'lambda env)
         (== `(closure ,x ,body ,env) val))))))

(define not-in-envo
  (lambda (x env)
    (conde
      ((fresh (y v rest)
         (== `((,y . ,v) . ,rest) env)
         (=/= y x)
         (not-in-envo x rest)))
      ((== '() env)))))

(define proper-listo
  (lambda (exp env val)
    (conde
      ((== '() exp)
       (== '() val))
      ((fresh (a d t-a t-d)
         (== `(,a . ,d) exp)
         (== `(,t-a . ,t-d) val)
         (eval-expo a env t-a)
         (proper-listo d env t-d))))))

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
