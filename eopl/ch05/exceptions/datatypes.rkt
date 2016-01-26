#lang racket

(require eopl "../../env.rkt" "lang.rkt")
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
    (proc procedure?)))

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

(define-datatype continuation continuation?
  (end-cont)
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
  (try-cont
    (var symbol?)
    (handler-exp expression?)
    (env environment?)
    (cont continuation?))
  (raise-cont
    (cont continuation?))
)
