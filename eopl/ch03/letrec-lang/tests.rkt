#lang racket

(require eopl "../../env.rkt" "lang.rkt" "datatypes.rkt" "interpreter.rkt")

(define init-env
  (lambda ()
    (extend-env*
      '(i v x)
      (list (num-val 1) (num-val 5) (num-val 10))
      (empty-env))))

(define prog-1 "
letrec double (x)
       = if zero?(x) then 0 else -((double -(x,1)), -2)
in (double 6)
")

(define prog-2 "
(proc (f) (f (f 77)) proc (x) -(x,11))
")

(value-of-program (scan&parse prog-1) (init-env))
(value-of-program (scan&parse prog-2) (init-env))
