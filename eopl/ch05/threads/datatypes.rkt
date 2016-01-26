#lang racket

(require eopl "../../env.rkt" "lang.rkt" "store.rkt")
(provide (all-defined-out))

;(define-datatype program program?
;  (a-program
;    (exp expression?)))
;(define-datatype expression expression?
;  (const-exp
;    (num number?))
;  (diff-exp
;    (exp1 expression?)
;    (exp2 expression?))
;  (zero?-exp
;    (exp expression?))
;  (if-exp
;    (exp1 expression?)
;    (exp2 expression?)
;    (exp3 expression?))
;  (var-exp
;    (var symbol?))
;  (let-exp
;    (var symbol?)
;    (exp expression?)
;    (body expression?)))

(define-datatype expval expval?
  (num-val
    (num number?))
  (bool-val
    (bool boolean?))
  (proc-val
    (proc procedure?))
  (mutex-val
    (mutex mutex?)))

(define expval->num
  (lambda (val)
    (cases expval val
      (num-val (num) num)
      (else (error 'num-required)))))

(define expval->bool
  (lambda (val)
    (cases expval val
      (bool-val (bool) bool)
      (else (error 'bool-required)))))

(define expval->proc
  (lambda (val)
    (cases expval val
      (proc-val (proc) proc)
      (else (error 'proc-required)))))

(define expval->mutex
  (lambda (val)
    (cases expval val
      (mutex-val (mutex) mutex)
      (else (error 'mutex-required)))))

(define-datatype mutex mutex?
  (a-mutex
    (ref-to-closed? reference?)
    (ref-to-wait-queue reference?)))

(define-datatype continuation continuation?
  (zero-cont
    (cont continuation?))
  (let-cont
    (var symbol?)
    (body expression?)
    (env environment?)
    (cont continuation?))
  (if-cont
    (exp2 expression?)
    (exp3 expression?)
    (env environment?)
    (cont continuation?))
  (diff-cont-1
    (exp2 expression?)
    (env environment?)
    (cont continuation?))
  (diff-cont-2
    (val1 expval?)
    (cont continuation?))
  (rator-cont
    (rand expression?)
    (env environment?)
    (cont continuation?))
  (rand-cont
    (val expval?)
    (cont continuation?))
  (assign-cont
    (var symbol?)
    (env environment?)
    (cont continuation?))
  (begin-cont
    (exp1 expression?)
    (exps list?)
    (env environment?)
    (cont continuation?))
  (print-cont
    (cont continuation?))
  (spawn-cont
    (cont continuation?))
  (end-main-thread-cont)
  (end-subthread-cont)
  (wait-cont
    (cont continuation?))
  (signal-cont
    (cont continuation?))
)
