#lang racket

(provide (all-defined-out))

;; () -> Env
(define empty-env
  (lambda ()
    (lambda (var)
      (error 'no-binding-found))))

;; Var * SchemeVal * Env -> Env
(define extend-env
  (lambda (var val env)
    (lambda (search-var)
      (if (eqv? search-var var)
          val
          (apply-env env search-var)))))

;; Listof(Var) * Listof(SchemeVal) * Env -> Env
(define extend-env*
  (lambda (vars vals env)
    (foldr (lambda (var-val env)
             (extend-env (car var-val) (cdr var-val) env))
           env
           (map cons vars vals))))

;; Env * Var -> SchemeVal
(define apply-env
  (lambda (env var)
    (env var)))

(define environment? procedure?)
