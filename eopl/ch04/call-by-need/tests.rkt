#lang racket

(require eopl "../../env.rkt" "store.rkt" "lang.rkt" "datatypes.rkt" "interpreter.rkt")

(define init-env
  (lambda ()
    (extend-env*
      '(i v x)
      (list (num-val 1) (num-val 5) (num-val 10))
      (empty-env))))

(define prog-1 "
let swap = proc (x) proc (y)
             let temp = x
             in begin
                  set x = y;
                  set y = temp
                end
in let a = 33
   in let b = 44
      in begin
           ((swap a) b);
           -(a,b)
         end
")

(define prog-2 "
let b = 3
in let p = proc (x) proc (y)
             begin
               set x = 4;
               y
             end
   in ((p b) b)
")

(define prog-3 "
letrec infinite-loop (x) = (infinite-loop -(x,1))
in let f = proc (z) 11
   in (f (infinite-loop 0))
")

(value-of-program (scan&parse prog-1) (init-env))
(value-of-program (scan&parse prog-2) (init-env))
(value-of-program (scan&parse prog-3) (init-env))
