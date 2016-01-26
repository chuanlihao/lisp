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

(test "
letrec f (x y) = +(x,y)
       g (x) = +(x,x)
in (f print((g 3)) print(4))
")

(test "
let x = newref(17)
in let y = setref(x, 27)
   in deref(x)
")
