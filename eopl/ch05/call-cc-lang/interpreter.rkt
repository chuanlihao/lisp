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
                (apply-cont cont (proc-val (normal-proc var body env))))
      (call-exp (rator rand)
                (value-of/k rator env (call-cont-1 rand env cont)))
      (letrec-exp (p-name b-var p-body letrec-body)
                  (value-of/k letrec-body
                            (extend-env-rec p-name b-var p-body env)
                            cont))
      (call-cc-exp (exp)
        (value-of/k exp env (call-cc-cont cont)))
      (else (error 'unexpected-expression))
)))

(define apply-procedure/k
  (lambda (proc val cont)
    (cases proc-datatype proc
      (normal-proc (var body env)
        (value-of/k body (extend-env var val env) cont))
      (call-cc-proc (call-cc-cont)
        (apply-cont call-cc-cont val)))))

(define extend-env-rec
  (lambda (p-name b-var p-body saved-env)
    (letrec ((new-env (lambda (var)
                        (if (eqv? var p-name)
                            (proc-val (normal-proc b-var p-body new-env))
                            (saved-env var)))))
      new-env)))

(define apply-cont
  (lambda (cont val)
    (cont val)))

(define end-cont
  (lambda ()
    (lambda (val)
      (begin
        (display "End of computation.")
        (newline)
        val))))

(define zero-cont
  (lambda (cont)
    (lambda (val)
      (apply-cont cont (bool-val (zero? (expval->num val)))))))

(define if-cont
  (lambda (then-part else-part env cont)
    (lambda (val)
      (value-of/k (if (expval->bool val)
                    then-part
                    else-part)
                env
                cont))))

(define let-cont
  (lambda (var body env cont)
    (lambda (val)
      (value-of/k body (extend-env var val env) cont))))

(define diff-cont-1
  (lambda (exp2 env cont)
    (lambda (val1)
      (value-of/k exp2 env (diff-cont-2 val1 cont)))))

(define diff-cont-2
  (lambda (val1 cont)
    (lambda (val2)
      (apply-cont cont (num-val (- (expval->num val1) (expval->num val2)))))))

(define call-cont-1
  (lambda (rand env cont)
    (lambda (proc)
      (value-of/k rand env (call-cont-2 proc cont)))))

(define call-cont-2
  (lambda (proc cont)
    (lambda (rand)
      (apply-procedure/k (expval->proc proc) rand cont))))

(define call-cc-cont
  (lambda (cont)
    (lambda (val)
      (apply-procedure/k
        (expval->proc val)
        (proc-val (call-cc-proc cont))
        cont))))
