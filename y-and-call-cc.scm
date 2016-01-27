(define eval
  (lambda (e env cont)
    (cond
      ((number? e) (cont e))
      ((symbol? e) (cont (env e)))
      ((eqv? (car e) 'zero?)
       (eval (second e) env
             (lambda (result) (cont (zero? result)))))
      ((eqv? (car e) '+)
       (eval (second e) env
             (lambda (one-result)
               (eval (third e) env
                     (lambda (the-other-result)
                       (cont (+ one-result the-other-result)))))))
      ((eqv? (car e) '-)        ; evaluation order changed
       (eval (third e) env
             (lambda (result)
               (eval (list '+ (second e) (- result)) env cont))))
      ((eqv? (car e) 'if)
       (eval (second e) env
             (lambda (result)
               (eval ((if result third fourth) e) env cont))))
      ((eqv? (car e) 'lambda)
       (cont (make-procedure (car (second e)) (third e) env)))
      ((eqv? (car e) 'call/cc)
       (eval (second e) env
             (lambda (proc)
               (proc (lambda (val ignored-cont) (cont val)) cont))))
      (else
       (eval (car e) env
             (lambda (proc)
               (eval (second e) env
                     (lambda (val)
                       (proc val cont)))))))))

(define make-procedure
  (lambda (var body env)
    (lambda (val cont)          ; apply
      (eval body (extend-env var val env) cont))))

(define empty-env
  (lambda ()
    (lambda (var)
      (error 'undefined-variable))))

(define extend-env
  (lambda (var val saved-env)
    (lambda (search-var)
      (if (eqv? var search-var)
          val
          (saved-env search-var)))))

(define second cadr)
(define third caddr)
(define fourth cadddr)

(define test
  (lambda (prog)
    (eval prog (empty-env) (lambda (v) v))))

(test '(((lambda (fe)
           ((lambda (f) (f f))
            (lambda (f) (fe (lambda (v) ((f f) v))))))
         (lambda (fk)
           (lambda (n)
             (if (zero? n)
                 0
                 (+ 2 (fk (- n 1)))))))
        10))

(test '((lambda (k)
          (k (lambda (x) 123)))
        (call/cc (lambda (k) k))))
