#lang racket

(require eopl "env.rkt" "lang.rkt" "datatypes.rkt")
(provide (all-defined-out))

(define value-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp)
                 (value-of exp (empty-nameless-env))))))

(define value-of
  (lambda (exp env)
    (cases expression exp
      (const-exp (num)
                 (num-val num))
      (nameless-var-exp (num)
                        (apply-nameless-env env num))
      (diff-exp (exp1 exp2)
                (num-val (- (expval->num (value-of exp1 env))
                            (expval->num (value-of exp2 env)))))
      (zero?-exp (exp)
                 (bool-val (zero? (expval->num (value-of exp env)))))
      (if-exp (exp1 exp2 exp3)
              (if (expval->bool (value-of exp1 env))
                  (value-of exp2 env)
                  (value-of exp3 env)))
      (nameless-let-exp (exp body)
                        (value-of body
                                  (extend-nameless-env (value-of exp env) env)))
      (nameless-proc-exp (body)
                         (proc-val (procedure body env)))
      (call-exp (rator rand)
                (apply-procedure (expval->proc (value-of rator env))
                                 (value-of rand env)))
      (else (error 'unexpected-condition))
)))


(define procedure
  (lambda (body env)
    (lambda (val)
      (value-of body (extend-nameless-env val env)))))

(define apply-procedure
  (lambda (proc val)
    (proc val)))
