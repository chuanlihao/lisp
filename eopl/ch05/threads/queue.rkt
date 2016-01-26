#lang racket

(provide (all-defined-out))

(define empty-queue
  (lambda ()
    '()))

(define empty? null?)

(define enqueue
  (lambda (q v)
    (append q (list v))))

(define dequeue
  (lambda (q f)
    (f (car q) (cdr q))))
