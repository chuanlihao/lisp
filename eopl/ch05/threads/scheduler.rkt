#lang racket

(require "queue.rkt")

(provide (all-defined-out))

(define the-ready-queue 'uninitialized)
(define the-final-answer 'uninitialized)
(define the-max-time-slice 'uninitialized)
(define the-time-remaining 'uninitialized)

(define initialize-scheduler!
  (lambda (ticks)
    (set! the-ready-queue (empty-queue))
    (set! the-final-answer 'uninitialized)
    (set! the-max-time-slice ticks)
    (set! the-time-remaining the-max-time-slice)))

(define place-on-ready-queue!
  (lambda (th)
    (set! the-ready-queue
          (enqueue the-ready-queue th))))

(define run-next-thread
  (lambda ()
    (if (empty? the-ready-queue)
        the-final-answer
        (dequeue the-ready-queue
                 (lambda (first rest)
                   (set! the-ready-queue rest)
                   (set! the-time-remaining the-max-time-slice)
                   (first))))))

(define set-final-answer!
  (lambda (v)
    (set! the-final-answer v)))

(define time-expired?
  (lambda ()
    (zero? the-time-remaining)))

(define decrement-timer!
  (lambda ()
    (set! the-time-remaining (- the-time-remaining 1))))
