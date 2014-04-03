;; run below two expressions only for the first time
;(define original+ +)
;(define original- -)
;
;(define zero? zero?)
;(define zero 0)
;(define add1
;  (lambda (n)
;    (original+ n 1)))
;(define sub1
;  (lambda (n)
;    (original- n 1)))

;; run below four expressions only for the first time,
;; for helper function encode and decode.
;(define original-zero? zero?)
;(define original-zero 0)
;(define original- -)
;(define original+ +)

(define zero? null?)
(define zero '())
(define add1
  (lambda (n)
    (cons '() n)))
(define sub1 cdr)

(define encode
  (lambda (n)
    (if (original-zero? n)
        zero
        (add1 (encode (original- n 1))))))
(define decode
  (lambda (n)
    (if (zero? n)
        original-zero
        (original+ 1 (decode (sub1 n))))))


(define +
  (lambda (n m)
    (if (zero? m)
        n
        (add1 (+ n (sub1 m))))))

(define *
  (lambda (n m)
    (if (zero? m)
        zero
        (+ n (* n (sub1 m))))))
