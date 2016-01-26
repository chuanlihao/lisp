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

(define prog-4 "
let f = proc (x) -(x, -(raise 99, 1))
in -(try (f 33)
     catch (m) -(m,55), 1)
")

(define prog-5 "
let f = proc (x) -(1, raise raise raise 99)
in
  try
    try (f 44)
    catch (exec) (f 23)
  catch (exec) 11
")

(value-of-program (scan&parse prog-4))
(value-of-program (scan&parse prog-5))
