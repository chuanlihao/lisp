#lang racket

(require eopl "lang.rkt")
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

(define type-of-program
  (lambda (prog)
    (cases program prog
      (a-program (exp) (type-of exp (init-tenv))))))

(define type-of
  (lambda (exp tenv)
    (cases expression exp
      (const-exp (num) (int-type))
      (var-exp (var) (apply-tenv tenv var))
      (diff-exp (exp1 exp2)
        (let ((ty1 (type-of exp1 tenv))
              (ty2 (type-of exp2 tenv)))
          (check-equal-type! ty1 (int-type) exp1)
          (check-equal-type! ty2 (int-type) exp2)
          (int-type)))
      (zero?-exp (exp)
        (let ((ty1 (type-of exp tenv)))
          (check-equal-type! ty1 (int-type) exp)
          (bool-type)))
      (if-exp (exp1 exp2 exp3)
        (let ((ty1 (type-of exp1 tenv))
              (ty2 (type-of exp2 tenv))
              (ty3 (type-of exp3 tenv)))
          (check-equal-type! ty1 (bool-type) exp1)
          (check-equal-type! ty2 ty3 exp)
          ty2))
      (let-exp (var exp body)
        (let ((exptype (type-of exp tenv)))
          (type-of body (extend-tenv var exptype tenv))))
      (proc-exp (var var-type body)
        (let* ((new-tenv (extend-tenv var var-type tenv))
               (body-type (type-of body new-tenv)))
          (proc-type var-type body-type)))
      (call-exp (rator rand)
        (let ((rator-type (type-of rator tenv))
              (rand-type (type-of rand tenv)))
          (cases type rator-type
            (proc-type (arg-type result-type)
              (begin
                (check-equal-type! arg-type rand-type rand)
                result-type))
            (else (error 'not-a-procedure)))))
      (letrec-exp (p-result-type p-name b-var b-var-type p-body letrec-body)
        (let* ((expected-proc-type (proc-type b-var-type p-result-type))
               (body-tenv (extend-tenv p-name expected-proc-type tenv))
               (proc-tenv (extend-tenv b-var b-var-type body-tenv))
               (real-body-type (type-of letrec-body body-tenv))
               (real-proc-result-type (type-of p-body proc-tenv)))
          (check-equal-type! real-proc-result-type p-result-type p-body)
          real-body-type))
          
      (else (error 'unexpected-exp)))))

(define test
  (lambda (prog-text)
    (type-of-program (scan&parse prog-text))))
