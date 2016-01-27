#lang racket

(require eopl)
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
    (bool boolean?)))

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