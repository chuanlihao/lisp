#lang racket

(require eopl "lang.rkt" "inferrer.rkt")

(test "
letrec ? f(x: ?) = (f (f x)) in f
")
