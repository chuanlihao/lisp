#lang racket

(require eopl "../../env.rkt" "lang.rkt" "datatypes.rkt" "interpreter.rkt")

(define init-env
  (lambda ()
    (extend-env*
      '(i v x)
      (list (num-val 1) (num-val 5) (num-val 10))
      (empty-env))))

(define prog
  "let x = 7
   in let y = 2
      in let y = let x = -(x,1)
                 in -(x,y)
         in -(-(x,8),y)")

(value-of-program (scan&parse prog) (init-env))
