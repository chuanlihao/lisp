#lang racket

(require eopl "datatypes.rkt" "store.rkt" "queue.rkt" "scheduler.rkt")

(provide (all-defined-out))

(define new-mutex
  (lambda ()
    (a-mutex
      (newref #f)
      (newref (empty-queue)))))

(define wait-for-mutex
  (lambda (m th)
    (cases mutex m
      (a-mutex (ref-closed? ref-queue)
        (cond
          ((deref ref-closed?)
            (setref! ref-queue
                     (enqueue (deref ref-queue) th))
            (run-next-thread))
          (else
            (setref! ref-closed? #t)
            (th)))))))

(define signal-mutex
  (lambda (m th)
    (cases mutex m
      (a-mutex (ref-closed? ref-queue)
        (let ((closed? (deref ref-closed?))
              (queue (deref ref-queue)))
          (when closed?
                (if (empty? queue)
                    (setref! ref-closed? #f)
                    (dequeue queue
                             (lambda (first-th other-ths)
                               (place-on-ready-queue! first-th)
                               (setref! ref-queue other-ths)))))
          (th))))))
