#lang racket

(require eopl "lang.rkt" "checker.rkt")

(test "
letrec (int -> int) f (g: (int -> int)) = proc (x: int) (g x)
in ((f proc (x:int) -(x,1)) 10)
")
