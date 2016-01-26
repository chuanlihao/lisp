#lang racket

(require eopl "lang.rkt" "substitutions.rkt" "unifier.rkt")
(provide (all-defined-out))

(define check-equal-type!
  (lambda (ty1 ty2 exp)
    (when (not (equal? ty1 ty2))
      (display (format "Types didn't match: ~a != ~a in ~a~%"
                       (type-to-external-form ty1)
                       (type-to-external-form ty2)
                       exp))
      (error 'error-occurred))))

(define-datatype type-environment type-environment?
  (empty-tenv-record)
  (extended-tenv-record
    (sym symbol?)
    (type type?)
    (tenv type-environment?)))

(define empty-tenv empty-tenv-record)
(define extend-tenv extended-tenv-record)

(define apply-tenv
  (lambda (tenv search-sym)
    (cases type-environment tenv
      (empty-tenv-record ()
        (error 'unbound-variable))
      (extended-tenv-record (sym val old-env)
        (if (eqv? sym search-sym)
            val
            (apply-tenv old-env search-sym))))))

(define init-tenv
  (lambda ()
    (extend-tenv 'x (int-type)
      (extend-tenv 'v (int-type)
        (extend-tenv 'i (int-type)
          (empty-tenv))))))

(define otype->type
  (lambda (otype)
    (cases optional-type otype
      (no-type () (fresh-tvar-type))
      (a-type (ty) ty))))

(define fresh-tvar-type
  (let ((sn 0))
    (lambda ()
      (set! sn (+ sn 1))
      (tvar-type sn))))

(define-datatype answer answer?
  (an-answer
    (ty type?)
    (subst substitution?)))

(define type-of-program
  (lambda (prog)
    (cases program prog
      (a-program (exp)
        (cases answer (type-of exp (init-tenv) (empty-subst))
          (an-answer (ty subst)
            (apply-subst-to-type ty subst)))))))

(define type-of
  (lambda (exp tenv subst)
    (cases expression exp
      (const-exp (num)
        (an-answer (int-type) subst))
      (var-exp (var)
        (an-answer (apply-tenv tenv var) subst))
      (diff-exp (exp1 exp2)
        (cases answer (type-of exp1 tenv subst)
          (an-answer (ty1 subst)
            (cases answer (type-of exp2 tenv (unifier (int-type) ty1 subst exp1))
              (an-answer (ty2 subst)
                (an-answer (int-type)
                           (unifier (int-type) ty2 subst exp2)))))))
      (zero?-exp (exp)
        (cases answer (type-of exp tenv subst)
          (an-answer (ty subst)
            (an-answer (bool-type)
                       (unifier (int-type) ty subst exp)))))
      (if-exp (exp1 exp2 exp3)
        (cases answer (type-of exp1 tenv subst)
          (an-answer (ty1 subst)
            (cases answer (type-of exp2 tenv (unifier (bool-type) ty1 subst exp1))
              (an-answer (ty2 subst)
                (cases answer (type-of exp3 tenv subst)
                  (an-answer (ty3 subst)
                    (an-answer ty2 (unifier ty2 ty3 subst exp)))))))))
      (let-exp (var exp body)
        (cases answer (type-of exp tenv subst)
          (an-answer (ty subst)
            (type-of body (extend-tenv var ty tenv) subst))))
      (proc-exp (var var-type body)
        (let ((var-type (otype->type var-type)))
          (cases answer (type-of body (extend-tenv var var-type tenv) subst)
            (an-answer (body-type subst)
              (an-answer (proc-type var-type body-type) subst)))))
      (call-exp (rator rand)
        (cases answer (type-of rator tenv subst)
          (an-answer (rator-type subst)
            (cases answer (type-of rand tenv subst)
              (an-answer (rand-type subst)
                (let ((result-type (fresh-tvar-type)))
                  (an-answer result-type
                             (unifier rator-type
                                      (proc-type rand-type result-type)
                                      subst
                                      exp))))))))
      (letrec-exp (p-result-type p-name b-var b-var-type p-body letrec-body)
        (let* ((p-result-type (otype->type p-result-type))
               (b-var-type (otype->type b-var-type))
               (expected-proc-type (proc-type b-var-type p-result-type))
               (body-tenv (extend-tenv p-name expected-proc-type tenv))
               (proc-tenv (extend-tenv b-var b-var-type body-tenv)))
          (cases answer (type-of p-body proc-tenv subst)
            (an-answer (real-proc-result-type subst)
              (type-of letrec-body
                       body-tenv
                       (unifier p-result-type real-proc-result-type subst p-body))))))
      (else (error 'unexpected-exp)))))

(define test
  (lambda (prog-text)
    (type-of-program (scan&parse prog-text))))
