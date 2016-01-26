#lang racket

(require eopl "../../env.rkt" "lang.rkt" "datatypes.rkt")
(provide (all-defined-out))

(define value-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp)
                 (value-of/k exp (empty-env) (end-cont))))))

(define value-of/k
  (lambda (exp env cont)
    (cases expression exp
      (const-exp (num)
                 (apply-cont cont (num-val num)))
      (var-exp (var)
               (apply-cont cont (apply-env env var)))
      (diff-exp (exp1 exp2)
                (value-of/k exp1 env (diff-cont-1 exp2 env cont)))
      (zero?-exp (exp)
                 (value-of/k exp env (zero-cont cont)))
      (if-exp (exp1 exp2 exp3)
              (value-of/k exp1 env (if-cont exp2 exp3 env cont)))
      (let-exp (var exp body)
               (value-of/k exp env (let-cont var body env cont)))
      (proc-exp (var body)
                (apply-cont cont (proc-val (procedure var body env))))
      (call-exp (rator rand)
                (value-of/k rator env (rator-cont rand env cont)))
      (letrec-exp (p-name b-var p-body letrec-body)
                  (value-of/k letrec-body
                            (extend-env-rec p-name b-var p-body env)
                            cont))
      (try-exp (exp var handler-exp)
               (value-of/k exp env (try-cont var handler-exp env cont)))
      (raise-exp (exp)
                 (value-of/k exp env (raise-cont cont)))
      (else (error 'unexpected-expression)))))

(define procedure
  (lambda (var body env)
    (lambda (val cont)
      (value-of/k body (extend-env var val env) cont))))

(define apply-procedure/k
  (lambda (proc val cont)
    (proc val cont)))

(define extend-env-rec
  (lambda (p-name b-var p-body saved-env)
    (letrec ((new-env (lambda (var)
                        (if (eqv? var p-name)
                            (proc-val (procedure b-var p-body new-env))
                            (saved-env var)))))
      new-env)))

(define apply-cont
  (lambda (current-cont val)
    (cases continuation current-cont
      (end-cont ()
        (begin
          (display "End of computation.")
          (newline)
          val))
      (zero-cont (cont)
        (apply-cont cont (bool-val (zero? (expval->num val)))))
      (if-cont (then-part else-part env cont)
        (value-of/k (if (expval->bool val)
                        then-part
                        else-part)
                    env
                    cont))
      (let-cont (var body env cont)
        (value-of/k body (extend-env var val env) cont))
      (diff-cont-1 (exp2 env cont)
        (value-of/k exp2 env (diff-cont-2 val cont)))
      (diff-cont-2 (val1 cont)
        (apply-cont cont (num-val (- (expval->num val1) (expval->num val)))))
      (rator-cont (rand env cont)
        (value-of/k rand env (rand-cont val cont)))
      (rand-cont (proc cont)
        (apply-procedure/k (expval->proc proc) val cont))
      (try-cont (var handler-exp env cont)
        (apply-cont cont val))
      (raise-cont (cont)
        (apply-handler val cont))
)))

(define apply-handler
  (lambda (val current-cont)
    (cases continuation current-cont
      (end-cont ()
        (error 'uncaught-exception))
      (zero-cont (cont)
        (apply-handler val cont))
      (if-cont (then-part else-part env cont)
        (apply-handler val cont))
      (let-cont (var body env cont)
        (apply-handler val cont))
      (diff-cont-1 (exp2 env cont)
        (apply-handler val cont))
      (diff-cont-2 (val1 cont)
        (apply-handler val cont))
      (rator-cont (rand env cont)
        (apply-handler val cont))
      (rand-cont (proc cont)
        (apply-handler val cont))
      (try-cont (var handler-exp env cont)
        (value-of/k handler-exp
                    (extend-env var val env)
                    cont))
      (raise-cont (cont)
        (apply-handler val cont)))))
