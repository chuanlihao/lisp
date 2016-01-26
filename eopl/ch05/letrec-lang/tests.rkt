#lang racket

(require eopl "lang.rkt" "interpreter.rkt")


(define prog-1 "
letrec double (x)
       = if zero?(x) then 0 else -((double -(x,1)), -2)
in (double 6)
")

(define prog-2 "
(proc (f) (f (f 77)) proc (x) -(x,11))
")

(define prog-3 "
let x = 3 in x
")

(value-of-program (scan&parse prog-1))
(value-of-program (scan&parse prog-2))
(value-of-program (scan&parse prog-3))
