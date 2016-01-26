#lang racket

(require eopl "../../env.rkt" "store.rkt" "lang.rkt" "datatypes.rkt" "interpreter.rkt")

(define init-env
  (lambda ()
    (extend-env*
      '(i v x)
      (list (num-val 1) (num-val 5) (num-val 10))
      (empty-env))))

(define prog-1 "
let x = newref(22)
in let f = proc (z) let zz = newref(-(z,deref(x)))
                    in deref(zz)
   in -((f 66), (f 55))
")

(define prog-2 "
let x = newref(22)
in let f = proc (z) let zz = setref(x, z)
                    in -(z, -1)
   in -((f 32), deref(x))
")

(value-of-program (scan&parse prog-1) (init-env))
(value-of-program (scan&parse prog-2) (init-env))
