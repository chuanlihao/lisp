#lang racket

(require eopl
         "env.rkt"
         "lang.rkt"
         "datatypes.rkt"
         "translator.rkt"
         "interpreter.rkt")

(define prog-1 "
let p = let x = 37
        in proc (y)
           let z = -(y,x)
           in -(-(x,y),z)
in (p 5)
")

(value-of-program (translation-of-program (scan&parse prog-1)))
