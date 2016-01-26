#lang racket

(require eopl "data-structures.rkt" "cps-out-lang.rkt")
(provide (all-defined-out))

(define value-of-program
  (lambda (pgm)
    (cases cps-out-program pgm
      (cps-a-program (exp)
        (value-of/k exp (init-env) (end-cont))))))

(define value-of-simple-exp
  (lambda (exp env)
    (cases simple-expression exp
      (cps-const-exp (num)
        (num-val num))
      (cps-var-exp (var)
        (apply-env env var))
      (cps-diff-exp (exp1 exp2)
        (num-val (- (expval->num (value-of-simple-exp exp1 env))
                    (expval->num (value-of-simple-exp exp2 env)))))
      (cps-sum-exp (exps)
        (num-val (apply + (map (lambda (e)
                                 (expval->num (value-of-simple-exp e env)))
                               exps))))
      (cps-zero?-exp (exp)
        (bool-val (zero? (expval->num (value-of-simple-exp exp env)))))
      (cps-proc-exp (vars body)
        (proc-val (procedure vars body env)))
      (else (error 'unimplemented)))))

(define value-of/k
  (lambda (exp env cont)
    (cases tfexp exp
      (simple-exp->exp (simple)
        (apply-cont cont (value-of-simple-exp simple env)))
      (cps-let-exp (var simple body)
        (value-of/k body
                    (extend-env* (list var)
                                 (list (value-of-simple-exp simple env))
                                 env)
                    cont))
      (cps-letrec-exp (p-names b-varss p-bodies letrec-body)
        (value-of/k letrec-body
                    (extend-env-rec* p-names b-varss p-bodies env)
                    cont))
      (cps-if-exp (simple then-part else-part)
        (value-of/k (if (expval->bool (value-of-simple-exp simple env))
                        then-part
                        else-part)
                    env
                    cont))
      (cps-call-exp (rator rands)
        (apply-procedure/k
          (expval->proc (value-of-simple-exp rator env))
          (map (lambda (s) (value-of-simple-exp s env)) rands)
          cont))
      (else (error 'unimplemented)))))

(define apply-cont
  (lambda (cont val)
    (cases continuation cont
      (end-cont () val))))

(define apply-procedure/k
  (lambda (proc-1 vals cont)
    (cases proc proc-1
      (procedure (vars body env)
        (value-of/k body (extend-env* vars vals env) cont)))))
