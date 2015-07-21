
(define MAKE-PAIR
  (lambda (x)
    (lambda (y)
      (lambda (f)
        ((f x) y)))))

(define LEFT
  (lambda (p)
    (p (lambda (x) (lambda (y) x)))))

(define RIGHT
  (lambda (p)
    (p (lambda (x) (lambda (y) y)))))


(define TRUE (lambda (x) (lambda (y) x)))
(define FALSE (lambda (x) (lambda (y) y)))

(define to-lisp-boolean
  (lambda (boolvalue) ((boolvalue #t) #f)))

(define NOT
  (lambda (v)
    (lambda (x)
      (lambda (y)
        ((v y) x)))))

(define AND
  (lambda (u)
    (lambda (v)
      (lambda (x)
        (lambda (y)
          ((u ((v x) y)) y))))))

(define OR
  (lambda (u)
    (lambda (v)
      (lambda (x)
        (lambda (y)
          ((u x) ((v x) y)))))))


(define Z
  (lambda (fe)
    ((lambda (f) (f f))
     (lambda (f)
       (fe (lambda (x) ((f f) x)))))))


(define ZERO (lambda (f) (lambda (x) x)))
(define ONE (lambda (f) (lambda (x) (f x))))
(define THREE (lambda (f) (lambda (x) (f (f (f x))))))
(define FIVE (lambda (f) (lambda (x) (f (f (f (f (f x))))))))

(define to-lisp-number
  (lambda (number) ((number (lambda (x) (+ x 1))) 0)))

(define INCREASE
  (lambda (n)
    (lambda (f)
      (lambda (x)
        (f ((n f) x))))))

(define SLIDE
  (lambda (p)
    ((MAKE-PAIR (RIGHT p)) (INCREASE (RIGHT p)))))

(define DECREASE
  (lambda (n)
    (LEFT ((n SLIDE) ((MAKE-PAIR ZERO) ZERO)))))

(define ADD
  (lambda (m)
    (lambda (n)
      ((n INCREASE) m))))

(define SUBTRACT
  (lambda (m)
    (lambda (n)
      ((n DECREASE) m))))

(define MULTIPLE
  (lambda (m)
    (lambda (n)
      ((m (ADD n)) ZERO))))

(define ZERO?
  (lambda (n)
    ((n (lambda (x) FALSE)) TRUE)))

(define LESS-EQUAL?
  (lambda (m)
    (lambda (n)
      (ZERO? ((SUBTRACT m) n)))))

(define LESS?
  (lambda (m)
    (lambda (n)
      (NOT ((LESS-EQUAL? n) m)))))

(define MOD
  (Z (lambda (f)
       (lambda (m)
         (lambda (n)
           ((((LESS? m) n) m) (lambda (x) (((f ((SUBTRACT m) n)) n) x)) ))))))


(define EMPTY-LIST
  ((MAKE-PAIR ((MAKE-PAIR FALSE) 0)) ZERO))

(define CONS
  (lambda (v)
    (lambda (l)
      ((MAKE-PAIR ((MAKE-PAIR TRUE) v)) l))))

(define CAR
  (lambda (l)
    (RIGHT (LEFT l))))

(define CDR RIGHT)

(define EMPTY?
  (lambda (l)
    (NOT (LEFT (LEFT l)))))

(define MAP
  (Z (lambda (fz)
       (lambda (f)
         (lambda (l)
           (((EMPTY? l)
             EMPTY-LIST)
             (lambda (x) (((CONS (f (CAR l))) ((fz f) (CDR l))) x))))))))


(define to-lisp-list
  (lambda (l)
    (if (to-lisp-boolean (EMPTY? l))
        '()
        (cons (CAR l) (to-lisp-list (CDR l))))))


(define MAKE-RANGE
  (Z (lambda (f)
       (lambda (m)
         (lambda (n)
           ((((LESS? n) m)
             EMPTY-LIST)
             (lambda (x) (((CONS m) ((f (INCREASE m)) n)) x))))))))


(define TWO (INCREASE ONE))
(define TEN ((MULTIPLE TWO) FIVE))
(define FIFTEEN ((MULTIPLE THREE) FIVE))
(define HUNDRED ((MULTIPLE TEN) TEN))
(define FUZZ (INCREASE HUNDRED))
(define BUZZ (INCREASE FUZZ))
(define FUZZBUZZ (INCREASE BUZZ))

(define CONVERT
  (lambda (n)
    (((ZERO? ((MOD n) FIFTEEN))
      FUZZBUZZ)
      (((ZERO? ((MOD n) THREE))
        FUZZ)
        (((ZERO? ((MOD n) FIVE))
          BUZZ)
          n)))))

(define FUZZBUZZLIST
  ((MAP CONVERT)
   ((MAKE-RANGE ONE) HUNDRED)))

(define fuzz-lisp-list
  (map to-lisp-number (to-lisp-list FUZZBUZZLIST)))

