#lang racket

(require eopl)
(provide (all-defined-out))

(define cps-lexical-spec
  '((whitespace (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier
      (letter (arbno (or letter digit "_" "-" "?")))
      symbol)
    (number (digit (arbno digit)) number)
    (number ("-" digit (arbno digit)) number)
))

(define cps-out-grammar
  '((cps-out-program
      (tfexp)
      cps-a-program)
    (simple-expression
      (number)
      cps-const-exp)
    (simple-expression
      (identifier)
      cps-var-exp)
    (simple-expression
      ("-" "(" simple-expression "," simple-expression ")")
      cps-diff-exp)
    (simple-expression
      ("+" "(" (separated-list simple-expression ",") ")")
      cps-sum-exp)
    (simple-expression
      ("zero?" "(" simple-expression ")")
      cps-zero?-exp)
    (simple-expression
      ("proc" "(" (arbno identifier) ")" tfexp)
      cps-proc-exp)
    (tfexp
      (simple-expression)
      simple-exp->exp)
    (tfexp
      ("let" identifier "=" simple-expression "in" tfexp)
      cps-let-exp)
    (tfexp
      ("letrec" (arbno identifier "(" (arbno identifier) ")" "=" tfexp)
       "in" tfexp)
      cps-letrec-exp)
    (tfexp
      ("if" simple-expression "then" tfexp "else" tfexp)
      cps-if-exp)
    (tfexp
      ("(" simple-expression (arbno simple-expression) ")")
      cps-call-exp)))
(sllgen:make-define-datatypes cps-lexical-spec cps-out-grammar)

(define cps-show-the-datatypes
  (lambda () (sllgen:list-define-datatypes cps-lexical-spec cps-out-grammar)))

(define cps-scan&parse
  (sllgen:make-string-parser cps-lexical-spec cps-out-grammar))

(define cps-just-scan
  (sllgen:make-string-scanner cps-lexical-spec cps-out-grammar))
