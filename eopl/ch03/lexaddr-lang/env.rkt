#lang racket

(provide (all-defined-out))

; nameless environment
(define nameless-env? list?)

(define empty-nameless-env
  (lambda ()
    '()))

(define extend-nameless-env cons)

(define apply-nameless-env list-ref)
