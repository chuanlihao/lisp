#lang racket

(require eopl "cps-out-lang.rkt")
(provide (all-defined-out))

(define-datatype expval expval?
  (num-val
    (value number?))
  (bool-val
    (value boolean?))
  (proc-val
    (val proc?)))

(define expval->num
  (lambda (v)
    (cases expval v
      (num-val (num) num)
      (else (error 'number-expected)))))

(define expval->bool
  (lambda (v)
    (cases expval v
      (bool-val (bool) bool)
      (else (error 'boolean-expected)))))

(define expval->proc
  (lambda (v)
    (cases expval v
      (proc-val (proc) proc)
      (else (error 'procedure-required)))))

(define-datatype proc proc?
  (procedure
    (vars (list-of symbol?))
    (body tfexp?)
    (env environment?)))

(define-datatype continuation continuation?
  (end-cont))

(define empty-env
  (lambda ()
    '()))

(define empty-env? null?)

(define extend-env*
  (lambda (syms vals old-env)
    (cons (list 'let syms vals) old-env)))

(define extend-env-rec*
  (lambda (p-names b-varss p-bodies old-env)
    (cons (list 'letrec p-names b-varss p-bodies) old-env)))

(define apply-env
  (lambda (env search-sym)
    (if (null? env)
        (error 'no-binding-found)
        (let* ((binding (car env))
               (saved-env (cdr env))
               (pos (list-index search-sym (cadr binding))))
          (if pos
              (case (car binding)
                ((let)
                 (list-ref (caddr binding) pos))
                ((letrec)
                 (let ((bvarss (caddr binding))
                       (bodies (cadddr binding)))
                   (proc-val
                     (procedure
                       (list-ref bvarss pos)
                       (list-ref bodies pos)
                       env)))))
              (apply-env saved-env search-sym))))))

(define list-index
  (lambda (sym los)
    (let loop ((pos 0) (los los))
      (cond ((null? los) #f)
            ((eqv? sym (car los)) pos)
            (else (loop (+ pos 1) (cdr los)))))))

(define environment?
  (list-of
    (lambda (p)
      (and (pair? p)
           (or (eqv? (car p) 'let)
               (eqv? (car p) 'letrec))))))

(define init-env
  (lambda ()
    (extend-env*
      '(i v x)
      (list (num-val 1) (num-val 5) (num-val 10))
      (empty-env))))
