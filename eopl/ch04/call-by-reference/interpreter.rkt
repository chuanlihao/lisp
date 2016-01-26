#lang racket

(require eopl "../../env.rkt" "store.rkt" "lang.rkt" "datatypes.rkt")
(provide (all-defined-out))

(define value-of-program
  (lambda (pgm env)
    (initialize-store!)
    (cases program pgm
      (a-program (exp)
                 (value-of exp env)))))

(define value-of
  (lambda (exp env)
    (cases expression exp
      (const-exp (num)
                 (num-val num))
      (var-exp (var)
               (deref (apply-env env var)))
      (diff-exp (exp1 exp2)
                (num-val (- (expval->num (value-of exp1 env))
                            (expval->num (value-of exp2 env)))))
      (zero?-exp (exp)
                 (bool-val (zero? (expval->num (value-of exp env)))))
      (if-exp (exp1 exp2 exp3)
              (if (expval->bool (value-of exp1 env))
                  (value-of exp2 env)
                  (value-of exp3 env)))
      (let-exp (var exp body)
               (value-of body
                         (extend-env var (newref (value-of exp env)) env)))
      (proc-exp (var body)
                (proc-val (procedure var body env)))
      (call-exp (rator rand)
                (apply-procedure (expval->proc (value-of rator env))
                                 (cases expression rand
                                   (var-exp (var) (apply-env env var))
                                   (else (newref (value-of rand env))))))
      (letrec-exp (p-name b-var p-body letrec-body)
                  (value-of letrec-body
                            (extend-env-rec p-name b-var p-body env)))
      (begin-exp (exp1 exps)
                 (last (map (lambda (e) (value-of e env))
                            (cons exp1 exps))))
      (assign-exp (var exp)
                  (begin
                    (setref! (apply-env env var)
                             (value-of exp env))
                    (num-val 27)))
)))


(define procedure
  (lambda (var body env)
    (lambda (val)
      (value-of body (extend-env var val env)))))

(define apply-procedure
  (lambda (proc val)
    (proc val)))

(define extend-env-rec
  (lambda (p-name b-var p-body saved-env)
    (letrec ((new-env (lambda (var)
                        (if (eqv? var p-name)
                            p-ref
                            (saved-env var))))
             (p-ref (newref (proc-val (procedure b-var p-body new-env)))))
      new-env)))
