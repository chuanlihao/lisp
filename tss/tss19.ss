(define call/cc call-with-current-continuation)

(define identity
  (lambda (x) x))

(define atom?
  (lambda (x)
    (and (not (null? x)) (not (pair? x)))))

(define sub1
  (lambda (n)
    (- n 1)))

(define deep
  (lambda (m)
    (if (zero? m)
        'pizza
        (cons (deep (sub1 m))
              '()))))

(define toppings #f)

(define deepB
  (lambda (m)
    (if (zero? m)
        (call/cc
          (lambda (cc)
            (set! toppings cc)
            'pizza))
        (cons (deepB (sub1 m)) '()))))

;(deepB 2)  --> ((pizza))
;(cons (toppings 'cake) (toppings 'fruit))  --> ((cake))

(define deep-cbB
  (lambda (m k)
    (if (zero? m)
        (begin
          (set! toppings k)
          (k 'pizza))
        (deep-cbB (sub1 m)
                  (lambda (x)
                    (k (cons x '())))))))

;(deep-cbB 2 identity)  --> ((pizza))
;(cons (toppings 'cake) (toppings 'fruit))  --> (((cake)) (fruit))

(define two-in-a-row?
  (letrec
      ((W (lambda (a lat)
            (if (null? lat)
                #f
                (or (eq? a (car lat))
                    (W (car lat) (cdr lat)))))))
    (lambda (lat)
      (if (null? lat)
          #f
          (W (car lat) (cdr lat))))))


(define leave #f)
(define fill (lambda (x) '()))

(define waddle
  (lambda (l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (call/cc
         (lambda (rest)
           (set! fill rest)
           (leave (car l))))
       (waddle (cdr l)))
      (else
        (waddle (car l))
        (waddle (cdr l))))))

(define get-first
  (lambda (l)
    (call/cc
      (lambda (here)
        (set! leave here)
        (waddle l)
        (leave '())))))

(define get-next
  (lambda (x)
    (call/cc
      (lambda (here-again)
        (set! leave here-again)
        (fill 'go)))))

(define two-in-a-row*?
  (letrec
      ((T? (lambda (a)
             (let ((n (get-next 'go)))
               (if (atom? n)
                   (or (eq? a n) (T? n))
                   #f)))))
    (lambda (l)
      (let ((fst (get-first l)))
        (if (atom? fst)
            (T? fst)
            #f)))))
