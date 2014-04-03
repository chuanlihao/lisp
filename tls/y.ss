(define add1
  (lambda (n) (+ n 1)))

(define eternity
  (lambda (x)
    (eternity x)))

(define length
  (lambda (l)
    (if (null? l)
        0
        (add1 (length (cdr l))))))

(define length0
  (lambda (l)
    (cond
      ((null? l) 0)
      (else (add1 (eternity (cdr l)))))))

(define length1
  (lambda (l)
    (cond
      ((null? l) 0)
      (else (add1 (length0 (cdr l)))))))

(((lambda (f)
    (lambda (l)
      (if (null? l)
          0
          (add1 ((f f) (cdr l))))))
  (lambda (f)
    (lambda (l)
      (if (null? l)
          0
          (add1 ((f f) (cdr l)))))))
 '(a b c d e))



(define Y
  (lambda (fe)
    ((lambda (f) (f f))
     (lambda (f)
       (fe (lambda(x) ((f f) x)))))))
       ;(fe (lambda(x y) ((f f) x y)))))))

(define length-e
  (lambda (f)
    (lambda (l)
      (if (null? l)
          0
          (+ 1 (f (cdr l)))))))

(define length-ee
  (lambda (f)
    (lambda (n l)
      (if (null? l)
          n
          (f (+ n 1) (cdr l))))))
