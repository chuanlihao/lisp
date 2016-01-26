#lang racket

(require eopl "lang.rkt" "datatypes.rkt")
(provide (all-defined-out))

(define empty-senv
  (lambda ()
    '()))

(define extend-senv
  (lambda (senv var)
    (cons var senv)))

(define apply-senv
  (lambda (senv var)
    (cond
      ((null? senv) (error 'unbound-var))
      ((eqv? var (car senv)) 0)
      (else (+ 1 (apply-senv (cdr senv) var))))))

(define translation-of-program
  (lambda (prog)
    (cases program prog
      (a-program (exp)
                 (a-program (translation-of (empty-senv) exp))))))

(define translation-of
  (lambda (senv exp)
    (cases expression exp
      (const-exp (num)
                 (const-exp num))
      (diff-exp (exp1 exp2)
                (diff-exp (translation-of senv exp1)
                          (translation-of senv exp2)))
      (zero?-exp (exp) (zero?-exp (translation-of senv exp)))
      (if-exp (exp1 exp2 exp3)
              (if-exp (translation-of senv exp1)
                      (translation-of senv exp2)
                      (translation-of senv exp3)))
      (var-exp (var)
               (nameless-var-exp (apply-senv senv var)))
      (let-exp (var exp body)
               (nameless-let-exp (translation-of senv exp)
                                 (translation-of (extend-senv senv var)
                                                 body)))
      (proc-exp (var body)
                (nameless-proc-exp (translation-of (extend-senv senv var)
                                                   body)))
      (call-exp (rator rand)
                (call-exp (translation-of senv rator)
                          (translation-of senv rand)))
      (else (error 'invalid-source-expression)))))


