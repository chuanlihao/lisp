#lang racket

(define zero
  (lambda () 0))

(define is-zero? zero?)

(define successor
  (lambda (n) (+ n 1)))

(define predecessor
  (lambda (n) (- n 1)))


;(define zero
;  (lambda () '()))
;
;(define is-zero? null?)
;
;(define successor
;  (lambda (n) (cons #t n)))
;
;(define predecessor cdr)


(define plus
  (lambda (x y)
    (if (is-zero? x)
        y
        (plus (predecessor x) (successor y)))))


;;; () -> Env
;(define empty-env
;  (lambda ()
;    (list 'empty-env)))
;
;;; Var * SchemeVal * Env -> Env
;(define extend-env
;  (lambda (var val env)
;    (list 'extend-env var val env)))
;
;;; Env * Var -> SchemeVal
;(define apply-env
;  (lambda (env search-var)
;    (cond
;      ((eqv? (car env) 'empty-env)
;       (error 'searching-value-not-found))
;      ((eqv? (car env) 'extend-env)
;       (if (eqv? (cadr env) search-var)
;           (caddr env)
;           (apply-env (cadddr env) search-var)))
;      (else
;       (error 'invalid-env)))))

; () -> Env
(define empty-env
  (lambda ()
    (lambda (search-var)
      (error 'no-binding-found))))

; Var * SchemeVal * Env -> Env
(define extend-env
  (lambda (var val env)
    (lambda (search-var)
      (if (eqv? search-var var)
          val
          (apply-env env search-var)))))

; Env * Var -> SchemeVal
(define apply-env
  (lambda (env var)
    (env var)))


;; constructors
;(define var-exp
;  (lambda (var)
;    (list 'var var)))
;(define lambda-exp
;  (lambda (var body)
;    (list 'lambda var body)))
;(define app-exp
;  (lambda (rator rand)
;    (list 'app rator rand)))
;
;; predicates
;(define var-exp?
;  (lambda (exp)
;    (first-symbol-is? 'var exp)))
;(define lambda-exp?
;  (lambda (exp)
;    (first-symbol-is? 'lambda exp)))
;(define app-exp?
;  (lambda (exp)
;    (first-symbol-is? 'app exp)))
;(define first-symbol-is?
;  (lambda (sym lst)
;    (eqv? sym (car lst))))
;
;; extractors
;(define var-exp->var cadr)
;(define lambda-exp->bound-var cadr)
;(define lambda-exp->body caddr)
;(define app-exp->rator cadr)
;(define app-exp->rand caddr)
;
;; Var * LcExp -> Bool
;(define occurs-free?
;  (lambda (search-var exp)
;    (cond
;      ((var-exp? exp) (eqv? search-var (var-exp->var exp)))
;      ((lambda-exp? exp)
;       (and (not (eqv? search-var (lambda-exp->bound-var exp)))
;            (occurs-free? search-var (lambda-exp->body exp))))
;      (else (or (occurs-free? search-var (app-exp->rator exp))
;                (occurs-free? search-var (app-exp->rand exp)))))))

(require eopl)

(define identifier? symbol?)

(define-datatype lc-exp lc-exp?
  (var-exp
    (var identifier?))
  (lambda-exp
    (bound-var identifier?)
    (body lc-exp?))
  (app-exp
    (rator lc-exp?)
    (rand lc-exp?)))

(define occurs-free?
  (lambda (search-var exp)
    (cases lc-exp exp
      (var-exp (var) (eqv? var search-var))
      (lambda-exp (bound-var body)
                  (and (not (eqv? search-var bound-var))
                       (occurs-free? search-var body)))
      (app-exp (rator rand)
               (or (occurs-free? search-var rator)
                   (occurs-free? search-var rand))))))
; SchemeVal -> LcExp
(define parse-expression
  (lambda (datum)
    (cond
      ((symbol? datum) (var-exp datum))
      ((eqv? (car datum) 'lambda)
       (lambda-exp (caadr datum)
                   (parse-expression (caddr datum))))
      (else (app-exp (parse-expression (car datum))
                     (parse-expression (cadr datum)))))))

; LcExp -> SchemeVal
(define unparse-lc-exp
  (lambda (exp)
    (cases lc-exp exp
      (var-exp (var) var)
      (lambda-exp (var body)
                  (list 'lambda (list var) (unparse-lc-exp body)))
      (app-exp (rator rand)
               (list (unparse-lc-exp rator) (unparse-lc-exp rand))))))


(define-datatype s-list s-list?
  (empty-s-list)
  (non-empty-s-list
    (first s-exp?)
    (rest s-list?)))

(define-datatype s-exp s-exp?
  (symbol-s-exp
    (sym symbol?))
  (s-list-s-exp
    (slst s-list?)))


(define-datatype bintree bintree?
  (leaf-node
    (num integer?))
  (interior-node
    (key symbol?)
    (left bintree?)
    (right bintree?)))

(define bintree-to-list
  (lambda (tree)
    (cases bintree tree
      (leaf-node (num) (list 'leaf-node num))
      (interior-node (key left right)
                     (list 'interior-node
                           key
                           (bintree-to-list left)
                           (bintree-to-list right))))))


(define-datatype prefix-list prefix-list?
  (a-prefix-list
    (prefix-exp prefix-exp?)))

(define-datatype prefix-exp prefix-exp?
  (atom-exp
    (atom-exp integer?))
  (compose-exp
    (left-exp prefix-exp?)
    (right-exp prefix-exp?)))

; SchemeList -> PrefixList
(define parse-prefix-list
  (lambda (lst)
    (a-prefix-list (car (parse-prefix-exp lst)))))

; SchemeList -> ( PrefixExp . SchemeList )
(define parse-prefix-exp
  (lambda (lst)
    (if (integer? (car lst))
        (cons (atom-exp (car lst))
              (cdr lst))
        (let* ((left (parse-prefix-exp (cdr lst)))
               (right (parse-prefix-exp (cdr left))))
          (cons (compose-exp (car left) (car right))
                (cdr right))))))
