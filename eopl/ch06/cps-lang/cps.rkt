#lang racket

(require eopl "cps-in-lang.rkt" "cps-out-lang.rkt")
(provide (all-defined-out))

(define fresh-identifier
  (let ((sn 0))
    (lambda (identifier)
      (set! sn (+ sn 1))
      (string->symbol
        (string-append
          (symbol->string identifier)
          "%"
          (number->string sn))))))

(define list-set
  (lambda (lst n val)
    (cond ((null? lst) (error 'list-set-ran-off-end))
          ((zero? n) (cons val (cdr lst)))
          (else (cons (car lst) (list-set (cdr lst) (- n 1) val))))))

(define list-index
  (lambda (pred lst)
    (cond
      ((null? lst) #f)
      ((pred (car lst)) 0)
      ((list-index pred (cdr lst)) => (lambda (n) (+ n 1)))
      (else #f))))

(define inp-exp-simple?
  (lambda (exp)
    (cases expression exp
      (const-exp (num) #t)
      (var-exp (var) #t)
      (proc-exp (ids exp) #t)
      (diff-exp (exp1 exp2)
                (and (inp-exp-simple? exp1)
                     (inp-exp-simple? exp2)))
      (zero?-exp (exp)
                 (inp-exp-simple? exp))
      (sum-exp (exps)
               (all-simple? exps))
      (else #f))))

(define all-simple?
  (lambda (exps)
    (if (null? exps)
        #t
        (and (inp-exp-simple? (car exps))
             (all-simple? (cdr exps))))))

(define index-of-first-non-simple
  (lambda (exps)
    (list-index (lambda (exp) (not (inp-exp-simple? exp))) exps)))

(define cps-of-simple-exp
  (lambda (exp)
    (cases expression exp
      (const-exp (num) (cps-const-exp num))
      (var-exp (var) (cps-var-exp var))
      (diff-exp (exp1 exp2)
                (cps-diff-exp (cps-of-simple-exp exp1)
                              (cps-of-simple-exp exp2)))
      (zero?-exp (exp)
                 (cps-zero?-exp (cps-of-simple-exp exp)))
      (proc-exp (vars exp)
                (cps-proc-exp (append vars (list 'k%00))
                              (cps-of-exp exp (cps-var-exp 'k%00))))
      (sum-exp (exps)
               (cps-sum-exp (map cps-of-simple-exp exps)))
      (else (error 'error-cps-of-simple-exp)))))

(define cps-of-exp
  (lambda (exp cont)
    (cases expression exp
      (const-exp (num) (make-send-to-cont cont (cps-of-simple-exp exp)))
      (var-exp (var) (make-send-to-cont cont (cps-of-simple-exp exp)))
      (proc-exp (vars body)
                (make-send-to-cont cont (cps-of-simple-exp exp)))
      (zero?-exp (exp) (cps-of-zero?-exp exp cont))
      (diff-exp (exp1 exp2) (cps-of-diff-exp exp1 exp2 cont))
      (sum-exp (exps) (cps-of-sum-exp exps cont))
      (if-exp (exp1 exp2 exp3) (cps-of-if-exp exp1 exp2 exp3 cont))
      (let-exp (var exp body) (cps-of-let-exp var exp body cont))
      (letrec-exp (p-names varss p-bodies body)
                  (cps-of-letrec-exp p-names varss p-bodies body cont))
      (call-exp (rator rands) (cps-of-call-exp rator rands cont))
      (else '(error 'error-cps-of-exp)))))

(define make-send-to-cont
  (lambda (cont exp)
    (cps-call-exp cont (list exp))))

(define cps-of-zero?-exp
  (lambda (exp cont)
    (cps-of-exps (list exp)
                 (lambda (new-rands)
                   (make-send-to-cont
                     cont
                     (cps-zero?-exp (car new-rands)))))))

(define cps-of-diff-exp
  (lambda (exp1 exp2 cont)
    (cps-of-exps (list exp1 exp2)
                 (lambda (new-rands)
                   (make-send-to-cont
                     cont
                     (cps-diff-exp (car new-rands)
                                   (cadr new-rands)))))))

(define cps-of-sum-exp
  (lambda (exps cont)
    (cps-of-exps exps
                 (lambda (new-rands)
                   (make-send-to-cont
                     cont
                     (cps-sum-exp new-rands))))))

(define cps-of-if-exp
  (lambda (exp1 exp2 exp3 cont)
    (cps-of-exps (list exp1)
                 (lambda (new-rands)
                   (cps-if-exp (car new-rands)
                               (cps-of-exp exp2 cont)
                               (cps-of-exp exp3 cont))))))

(define cps-of-let-exp
  (lambda (var exp body cont)
    (cps-of-exps (list exp)
                 (lambda (new-rands)
                   (cps-let-exp
                     var
                     (car new-rands)
                     (cps-of-exp body cont))))))

(define cps-of-letrec-exp
  (lambda (proc-names varss prod-bodies body cont)
    (cps-letrec-exp
      proc-names
      (map (lambda (vars) (append vars (list 'k%00))) varss)
      (map (lambda (exp) (cps-of-exp exp (cps-var-exp 'k%00))) prod-bodies)
      (cps-of-exp body cont))))

(define cps-of-call-exp
  (lambda (rator rands cont)
    (cps-of-exps (cons rator  rands)
                 (lambda (new-rands)
                   (cps-call-exp
                     (car new-rands)
                     (append (cdr new-rands) (list cont)))))))

(define cps-of-exps
  (lambda (exps builder)
    (let cps-of-rest ((exps exps))
      (let ((pos (index-of-first-non-simple exps)))
        (if pos
            (let ((var (fresh-identifier 'v)))
              (cps-of-exp
                (list-ref exps pos)
                (cps-proc-exp (list var)
                              (cps-of-rest (list-set exps pos (var-exp var))))))
            (builder (map cps-of-simple-exp exps)))))))

(define cps-of-program
  (lambda (prog)
    (cases program prog
      (a-program (exp)
        (cps-a-program
          (cps-of-exps (list exp)
                       (lambda (new-args)
                         (simple-exp->exp (car new-args)))))))))
