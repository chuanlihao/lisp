(define first car)
(define second cadr)
(define third caddr)
(define fourth cadddr)

(define empty-env
  (lambda ()
    (lambda (query)
      'error-empty-env)))

(define extend
  (lambda (var val env)
    (lambda (query)
      (if (eq? query var)
          val
          (env query)))))

(define multi-extend
  (lambda (vars vals env)
    (if (null? vars)
        env
        (multi-extend
          (cdr vars)
          (cdr vals)
          (extend
            (car vars)
            (car vals)
            env)))))

(define global-env
  (multi-extend
    '(null? cdr +)
    (list
      (lambda ()
        (lambda (args)
          (null? ((car args)))))
      (lambda ()
        (lambda (args)
          (cdr ((car args)))))
      (lambda ()
        (lambda (args)
          (+ ((first args)) ((second args))))))
    (empty-env)))

(define my-eval
  (lambda (exp env)
    (cond
      ((or (number? exp) (eq? exp #t) (eq? exp #f))
       (lambda () exp))
      ((symbol? exp)
       (lambda () ((env exp))))
      ((eq? (car exp) 'quote)
       (lambda () (second exp)))
      ((eq? (car exp) 'if)
       (lambda ()
         (if ((my-eval (second exp) env))
             ((my-eval (third exp) env))
             ((my-eval (fourth exp) env)))))
      ((eq? (car exp) 'lambda)
       (lambda ()
         (lambda (args)
           ((my-eval (third exp)
                     (multi-extend
                       (second exp)
                       args
                       env))))))
      (else
        (lambda ()
          (((my-eval (car exp) env))
           (map (lambda (arg)
                  (my-eval arg env))
                (cdr exp))))))))

((my-eval
   '(((lambda (fun)
        ((lambda (f) (f f))
         (lambda (f) (fun (f f)))))
      (lambda (len)
        (lambda (l)
          (if (null? l)
              0
              (+ 1 (len (cdr l)))))))
     '(a b c d e f g))
   global-env))            ; ==> 7

((my-eval
   '((lambda (x) 123) (cdr '()))
   global-env))            ; ==> 123
