#lang racket

(require eopl "lang.rkt" "store.rkt")
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
  (mutpair-val
    (pair mutpair?)))

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

(define expval->mutpair
  (lambda (val)
    (cases expval val
      (mutpair-val (pair) pair)
      (else (error 'mutpair-required)))))

(define-datatype mutpair mutpair?
  (a-pair
    (left-loc reference?)
    (right-loc reference?)))

(define make-pair
  (lambda (val1 val2)
    (a-pair (newref val1)
            (newref val2))))

(define left
  (lambda (p)
    (cases mutpair p
      (a-pair (left-loc right-loc)
              (deref left-loc)))))

(define right
  (lambda (p)
    (cases mutpair p
      (a-pair (left-loc right-loc)
              (deref right-loc)))))

(define setleft
  (lambda (p val)
    (cases mutpair p
      (a-pair (left-loc right-loc)
              (setref! left-loc val)))))

(define setright
  (lambda (p val)
    (cases mutpair p
      (a-pair (left-loc right-loc)
              (setref! right-loc val)))))
