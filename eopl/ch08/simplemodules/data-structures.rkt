#lang racket

(require eopl "lang.rkt")
(provide (all-defined-out))

(define-datatype environment environment?
  (empty-env)
  (extend-env
    (bvar symbol?)
    (bval expval?)
    (saved-env environment?))
  (extend-env-recursively
    (id symbol?)
    (bvar symbol?)
    (body expression?)
    (saved-env environment?))
  (extend-env-with-module
    (m-name symbol?)
    (m-val typed-module?)
    (saved-env environment?)))

(define-datatype typed-module typed-module?
  (simple-module
    (bindings environment?)))

(define-datatype proc proc?
  (procedure
    (bvar symbol?)
    (body expression?)
    (env environment?)))

(define-datatype expval expval?
  (num-val
    (value number?))
  (bool-val
    (value boolean?))
  (proc-val
    (value proc?)))

(define expval->num
  (lambda (v)
    (cases expval v
      (num-val (v) v)
      (else (error 'num-required)))))

(define expval->bool
  (lambda (v)
    (cases expval v
      (bool-val (v) v)
      (else (error 'bool-required)))))

(define expval->proc
  (lambda (v)
    (cases expval v
      (proc-val (v) v)
      (else (error 'proc-required)))))
