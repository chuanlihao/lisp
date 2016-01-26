#lang racket

(require eopl "data-structures.rkt")
(provide (all-defined-out))

(define apply-env
  (lambda (env search-sym)
    (cases environment env
      (empty-env ()
        (eopl:error 'apply-env "No value binding for ~s" search-sym))
      (extend-env (bvar bval saved-env)
        (if (eqv? search-sym bvar)
            bval
            (apply-env saved-env search-sym)))
      (extend-env-recursively (id bvar body saved-env)
        (if (eqv? search-sym id)
            (proc-val (procedure bvar body env))
            (apply-env saved-env search-sym)))
      (extend-env-with-module (m-name m-val saved-env)
        (apply-env saved-env search-sym)))))

(define lookup-module-name-in-env
  (lambda (m-name env)
    (cases environment env
      (empty-env ()
        (eopl:error 'lookup-module-name-in-env
                    "No module binding for ~s"
                    m-name))
      (extend-env (bvar bval saved-env)
        (lookup-module-name-in-env m-name saved-env))
      (extend-env-recursively (id bvar body saved-env)
        (lookup-module-name-in-env m-name saved-env))
      (extend-env-with-module (m-name1 m-val saved-env)
        (if (eqv? m-name m-name1)
            m-val
            (lookup-module-name-in-env m-name saved-env))))))

(define lookup-qualified-var-in-env
  (lambda (m-name var-name env)
    (let ((m-val (lookup-module-name-in-env m-name env)))
      (cases typed-module m-val
        (simple-module (bindings)
          (apply-env bindings var-name))))))

(define test-env
  (lambda ()
    (extend-env
      'a
      (num-val 10)
      (extend-env
        'b
        (num-val 20)
        (extend-env-with-module
          'm2
          (simple-module (extend-env
                           'x
                           (num-val 30)
                           (extend-env
                             'y
                             (num-val 40)
                             (empty-env))))
          (extend-env-with-module
            'm1
            (simple-module (extend-env
                             'u
                             (num-val 50)
                             (extend-env
                               'v
                               (num-val 60)
                               (empty-env))))
            (empty-env)))))))
