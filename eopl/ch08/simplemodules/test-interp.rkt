#lang racket


;(test "
;letrec (int -> int) f (g: (int -> int)) = proc (x: int) (g x)
;in ((f proc (x:int) -(x,1)) 10)
;")

(require eopl "lang.rkt" "interp.rkt")

(test "
module m1
  interface
    [ a : int
      b : int
      c : int ]
  body
    [ a = 33
      x = -( a , 1 )
      b = -( a , x )
      c = -( x , b ) ]
let a = 10
in -( -( from m1 take a , from m1 take b ) , a )
")

(test "
module m1
  interface [ u : int  v : int ]
  body [ u = 50  w = -10  v = -(u,w) ]
module m2
  interface [ x : int  y : int ]
  body [ x = 30  y = 40 ]
letrec int f ( x : int )
         = if zero?(x) then 0 else -((f -(x,1) ) , -2)
in let a = 10
   in let b = proc (x : int) -(x,-10)
      in (f -( -( from m2 take y , from m2 take x ),
            -( -((b a),a) , -( from m1 take v , from m1 take u ) ) ))
")
