#lang racket

(require eopl "lang.rkt" "data-structures.rkt" "env.rkt")
(provide (all-defined-out))

(define value-of-program
  (lambda (prog)
    (cases program prog
      (a-program (module-defs body)
        (value-of body
                  (add-module-defns-to-env module-defs (empty-env)))))))

(define add-module-defns-to-env
  (lambda (defs env)
    (if (null? defs)
        env
        (cases module-definition (car defs)
          (a-module-definition (m-name iface m-body)
            (add-module-defns-to-env
              (cdr defs)
              (extend-env-with-module
                m-name
                (value-of-module-body m-body env)
                env)))))))

(define value-of-module-body
  (lambda (m-body env)
    (cases module-body m-body
      (defns-module-body (defns)
        (simple-module
          (defns-to-env defns env))))))

(define defns-to-env
  (lambda (defns env)
    (if (null? defns)
        (empty-env)
        (cases definition (car defns)
          (val-defn (var exp)
            (let ((val (value-of exp env)))
              (extend-env
                var
                val
                (defns-to-env
                  (cdr defns)
                  (extend-env var val env)))))))))

(define value-of
  (lambda (exp env)
    (cases expression exp
      (const-exp (num) (num-val num))
      (var-exp (var) (apply-env env var))
      (qualified-var-exp (m-name var-name)
        (lookup-qualified-var-in-env m-name var-name env))
      (diff-exp (exp1 exp2)
        (num-val (- (expval->num (value-of exp1 env))
                    (expval->num (value-of exp2 env)))))
      (zero?-exp (exp)
        (bool-val (zero? (expval->num (value-of exp env)))))
      (if-exp (exp1 exp2 exp3)
        (value-of
          (if (expval->bool (value-of exp1 env)) exp2 exp3)
          env))
      (let-exp (var exp body)
        (value-of body (extend-env var (value-of exp env) env)))
      (proc-exp (bvar ty body)
        (proc-val (procedure bvar body env)))
      (call-exp (rator rand)
        (apply-procedure (expval->proc (value-of rator env))
                         (value-of rand env)))
      (letrec-exp (ty1 p-name bvar ty2 p-body letrec-body)
        (value-of letrec-body
                  (extend-env-recursively p-name bvar p-body env)))
      (else (error 'value-of-unexpected-exp)))))

(define apply-procedure
  (lambda (proc1 val)
    (cases proc proc1
      (procedure (var body saved-env)
        (value-of body (extend-env var val saved-env))))))

(define test
  (lambda (text)
    (value-of-program (scan&parse text))))
