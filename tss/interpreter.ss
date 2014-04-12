; basic functions
(define call/cc call-with-current-continuation)
(define first car)
(define second cadr)
(define third caddr)
(define atom?
  (lambda (x)
    (and (not (null? x)) (not (pair? x)))))
(define add1
  (lambda (x) (+ x 1)))
(define sub1
  (lambda (x) (- x 1)))

; utility functions
(define a-func
  (lambda (f)
    (lambda (args)
      (f (first args)))))

(define b-func
  (lambda (f)
    (lambda (args)
      (f (first args) (second args)))))

; box operations
(define box
  (lambda (it)
    (let ((set (lambda (new) (set! it new))))
      (lambda (op)
        (op it set)))))

(define unbox
  (lambda (box)
    (box (lambda (it set) it))))

(define setbox!
  (lambda (box new)
    (box (lambda (it set) (set new)))))

; table(environment) operations
(define extend-table
  (lambda (set-var set-val orig-table)
    (lambda (get-var)
      (if (eq? get-var set-var)
          set-val
          (orig-table get-var)))))

(define multi-extend-table
  (lambda (vars vals orig-table)
    (if (null? vars)
        orig-table
        (multi-extend-table
          (cdr vars)
          (cdr vals)
          (extend-table
            (car vars)
            (car vals)
            orig-table)))))

(define lookup-in-table
  (lambda (var table)
    (table var)))

; tables
(define the-empty-table
  (lambda (var)
    'var-not-found))

(define the-global-table         ; may call (set! the-global-table ...) later
  the-empty-table)

(define the-newest-global-table  ; the newest version of 'the-global-table'
  (lambda (var)
    (the-global-table var)))

; setup predefined functions in the global table
(set! the-global-table
  (multi-extend-table
    '(atom? eq? null? cons car cdr number? zero? add1 sub1)
    (list (box (a-func atom?))
          (box (b-func eq?))
          (box (a-func null?))
          (box (b-func cons))
          (box (a-func car))
          (box (a-func cdr))
          (box (a-func number?))
          (box (a-func zero?))
          (box (a-func add1))
          (box (a-func sub1)))
    the-global-table))

; evaluation
(define value
  (let ((define? (lambda (e)
                   (and (pair? e) (not (null? e)) (eq? 'define (car e))))))
    (lambda (expr)
      (if (define? expr)
          (*define expr)
          (my-eval expr the-newest-global-table)))))

(define my-eval
  (let ((atom-to-action
          (lambda (e)
            (if (or (number? e) (eq? e #t) (eq? e #f))
                *self
                *identifier)))
        (list-to-action
          (lambda (e)
            (cond
              ((eq? (car e) 'quote) *quote)
              ((eq? (car e) 'lambda) *lambda)
              ((eq? (car e) 'set!) *set!)
              ((eq? (car e) 'cond) *cond)
              ((eq? (car e) 'letcc) *letcc)
              (else *my-apply)))))
    (lambda (expr table)
      ((if (atom? expr)
           (atom-to-action expr)
           (list-to-action expr))
        expr
        table))))

; various actions
(define *self
  (lambda (e table) e))

(define *quote
  (lambda (e table)
    (second e)))

(define *identifier
  (lambda (e table)
    (unbox (lookup-in-table e table))))

(define *define
  (lambda (expr)
    (set! the-global-table
          (extend-table
            (second expr)
            (box (my-eval (third expr) the-newest-global-table))
            the-global-table))))

(define *set!
  (lambda (e table)
    (setbox!
      (lookup-in-table (second e) table)
      (my-eval (third e) table))))

(define *my-apply
  (lambda (e table)
    ((my-eval (first e) table)
     (map (lambda (arg) (my-eval arg table))
          (cdr e)))))

(define multi-eval
  (lambda (exprs table)
    (let ((first-val (my-eval (car exprs) table)))
      (if (null? (cdr exprs))
          first-val
          (multi-eval (cdr exprs) table)))))

(define *lambda
  (lambda (e table)
    (lambda (args)
      (multi-eval
        (cddr e)
        (multi-extend-table
          (second e)
          (map box args)
          table)))))

(define *cond
  (lambda (e table)
    (letrec
        ((C (lambda (es)
              (if (or (eq? (caar es) 'else)
                      (my-eval (caar es) table))
                  (multi-eval (cdar es) table)
                  (C (cdr es))))))
      (C (cdr e)))))

(define *letcc
  (lambda (e table)
    (call/cc
      (lambda (skip)
        (multi-eval
          (cddr e)
          (extend-table (second e) (box (a-func skip)) table))))))
