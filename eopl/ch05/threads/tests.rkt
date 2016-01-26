#lang racket

(require eopl "lang.rkt" "interpreter.rkt")

(define prog-1 "
letrec double (x)
       = if zero?(x) then 0 else -((double -(x,1)), -2)
in (double 6)
")

(define prog-2 "
let f = proc(x) proc(y)
          begin
            set x = -(x,1);
            -(x,y)
          end
in ((f 44) 33)
")

(define prog-3 "
(proc (f) (f (f 77)) proc (x) -(x,11))
")

(define prog-4 "
letrec x (a) = if zero?(a) then 0 else (x -(a, 1)) in (x 1)
")

(define prog-5 "
let x = 5 in -(set x = -(x, -10), x)
")

(define prog-6 "
letrec noisy (n) = if zero?(n)
                   then 0
                   else begin print(n); (noisy -(n,1)) end
in begin
     spawn(proc (d) (noisy 5));
     spawn(proc (d) (noisy 10));
     print (100);
     33
   end
")

(define prog-7 "
let buffer = 0
in let producer = proc (n)
         letrec
           wait(k) = if zero?(k)
                     then set buffer = n
                     else begin
                            print(-(k, -200));
                            (wait -(k,1))
                          end
         in (wait 5)
   in let consumer = proc (d)
            letrec busywait(k) = if zero?(buffer)
                                 then begin
                                        print(-(k,-100));
                                        (busywait -(k,-1))
                                      end
                                 else buffer
            in (busywait 0)
      in begin
           spawn(proc(d) (producer 44));
           print(300);
           (consumer 86)
         end
")

(define prog-8 "
let x = 0 in let mut = mutex()
   in letrec f (y) = if zero?(y) then 0 else (f -(y,1))
      in let incr_x = proc (id) proc (dummy)
                                 begin
                                   wait(mut);
                                   let y = x
                                   in let z = (f 1)
                                      in set x = -(y,-1);
                                   signal(mut)
                                 end
      in begin
           spawn((incr_x 100));
           spawn((incr_x 200));
           spawn((incr_x 300));
           (f 500);
           x
         end
")

(define test
  (lambda (n)
    (if (not (zero? n))
        (begin
          (display n)
          (display " ")
          (display (value-of-program n (scan&parse prog-8)))
          (newline)
          (test (- n 1)))
        #t)))
(test 50)
;(value-of-program 2 (scan&parse prog-2))
;(value-of-program 2 (scan&parse prog-3))
;(value-of-program 2 (scan&parse prog-4))
;(value-of-program 2 (scan&parse prog-5))
