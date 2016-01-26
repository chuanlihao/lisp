#lang racket

(provide (all-defined-out))

(define empty-store
  (lambda () '()))

(define the-store 'uninitialized)

(define get-store
  (lambda () the-store))

(define initialize-store!
  (lambda ()
    (set! the-store (empty-store))))

(define reference? integer?)

(define newref
  (lambda (val)
    (let ((next-ref (length the-store)))
      (set! the-store (append the-store (list val)))
      next-ref)))

(define deref
  (lambda (ref)
    (list-ref the-store ref)))

(define setref!
  (lambda (ref val)
    (set! the-store
          (letrec ((K (lambda (s r)
                        (cond
                          ((null? s) (error 'invalid-ref))
                          ((zero? r) (cons val (cdr s)))
                          (else (cons (car s)
                                      (K (cdr s) (- r 1))))))))
            (K the-store ref)))))
