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

(define prog-3 "
let glo = newpair(11,22)
in let f = proc (loc)
             begin
               setright loc = left(loc);
               setleft glo = 99;
               -(left(loc),right(loc))
             end
   in (f glo)
")

(value-of-program (scan&parse prog-1) (init-env))
(value-of-program (scan&parse prog-2) (init-env))
(value-of-program (scan&parse prog-3) (init-env))
