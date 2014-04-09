(define atom?
  (lambda (x)
    (and (not (null? x)) (not (pair? x)))))

(define L
  (lambda (f-length)
    (lambda (l)
      (if (null? l)
          0
          (+ 1 (f-length (cdr l)))))))

(define Y!
  (lambda (f)
    (let ((h (lambda (l) 0)))
      (set! h
        (f (lambda (arg) (h arg))))
      h)))

(define Y-bang
  (lambda (f)
    (letrec
      ((h (f (lambda (x) (h x)))))
      h)))

(define D
  (lambda (depth*)
    (lambda (l)
      (cond
        ((null? l) 1)
        ((atom? (car l))
         (depth* (cdr l)))
        (else (max (+ 1 (depth* (car l)))
                   (depth* (cdr l))))))))
(define depth1 (Y! D))
(define depth2 (Y-bang D))
