#lang racket

(require eopl "cps-in-lang.rkt" "cps.rkt" "interp.rkt")

(define test
  (lambda (prog-text)
    (value-of-program (cps-of-program (scan&parse prog-text)))))

(test "
letrec fib(n) = if zero?(n) then 1
                else if zero?(-(n,1)) then 1
                else +( (fib -(n,1)), (fib -(n,2)) )
in +((fib 1), 12, (fib 5))
")
