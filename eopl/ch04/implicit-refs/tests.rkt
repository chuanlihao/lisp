#lang racket

(require eopl "../../env.rkt" "store.rkt" "lang.rkt" "datatypes.rkt" "interpreter.rkt")

(define init-env
  (lambda ()
    (extend-env*
      '(i v x)
      (list (num-val 1) (num-val 5) (num-val 10))
      (empty-env))))

(define prog-1 "
let f = proc(x) proc(y)
          begin
            set x = -(x,1);
            -(x,y)
          end
in ((f 44) 33)
")

(define prog-2 "
(proc (f) (f (f 77)) proc (x) -(x,11))
")

(value-of-program (scan&parse prog-1) (init-env))
(value-of-program (scan&parse prog-2) (init-env))
