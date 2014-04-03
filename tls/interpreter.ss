(define first car)
(define second cadr)
(define third caddr)

(define atom?
  (lambda (e)
    (and (not (null? e)) (not (pair? e)))))

(define build-entry
  (lambda (keys vals)
    (cons keys (cons vals '()))))

(define expand-env cons)

(define search-env
  (lambda (identifier env)
    (if (null? env)
        (error 'cannot-find-identifier)
        (search-env-entry
          identifier
          (first (car env))
          (second (car env))
          (lambda ()
            (search-env identifier (cdr env)))))))

; here, CPS style is used, since two return values required, which are the value
; and whether the search succeeds.  Cannot combine these two return values, for
; the existence of #f.
(define search-env-entry
  (lambda (identifier keys vals fail-cb)
    (cond ((null? keys) (fail-cb))
          ((eq? (car keys) identifier) (car vals))
          (else (search-env-entry identifier (cdr keys) (cdr vals) fail-cb)))))

(define my-eval
  (lambda (expr env)
    ((exp-to-eval-functions expr) expr env)))

(define exp-to-eval-functions
  (lambda (expr)
    (cond
      ((null? expr) eval-self)
      ((number? expr) eval-self)
      ((boolean? expr) eval-self)
      ((atom? expr) eval-identifier)
      ((eq? (car expr) 'quote) eval-quote)
      ((eq? (car expr) 'cond) eval-condition)
      ((eq? (car expr) 'lambda) eval-definition)
      ((to-inner-function expr) => create-eval-inner-function-call)
      (else eval-custom-function-call))))

(define to-inner-function
  (lambda (expr)
    (cond 
      ((eq? (car expr) 'zero?) zero?)
      ((eq? (car expr) '+) +)
      ((eq? (car expr) '*) *)
      ((eq? (car expr) 'null?) null?)
      ((eq? (car expr) 'car) car)
      ((eq? (car expr) 'cdr) cdr)
      ((eq? (car expr) 'cons) cons)
      (else #f))))

(define eval-self
  (lambda (val env)
    val))

(define eval-identifier search-env)

(define eval-quote
  (lambda (quoted env)
    (second quoted)))

(define eval-condition
  (lambda (condition env)
    (eval-conds (cdr condition) env)))

(define eval-conds 
  (lambda (conds env)
    (cond ((null? conds) (error 'null-conditions))
          ((eq? (first (car conds)) 'else)
           (my-eval (second (car conds)) env))
          ((my-eval (first (car conds)) env)
           (my-eval (second (car conds)) env))
          (else (eval-conds (cdr conds) env)))))

; format (env args body)
(define eval-definition
  (lambda (def env)
    (cons env (cdr def))))

(define create-eval-inner-function-call
  (lambda (inner-function)
    (lambda (fun-call env)
      (apply inner-function (eval-args (cdr fun-call) env)))))

(define eval-custom-function-call
  (lambda (expr env)
    (call-apply
      (my-eval (car expr) env)
      (eval-args (cdr expr) env))))

(define call-apply
  (lambda (func args)
    (my-eval
      (third func)
      (expand-env
        (build-entry (second func) args)
        (first func)))))

(define eval-args
  (lambda (args env)
    (if (null? args)
        '()
        (cons (my-eval (car args) env)
              (eval-args (cdr args) env)))))

(define example-env
  (expand-env
    (build-entry '(x y) '(1 2))
    (expand-env
      (build-entry '(y z) '(22 33))
      '())))

(my-eval '((lambda (u v) (+ (* y u) v)) 3 4) example-env) ; --> 10

(my-eval '((lambda (func) (func)) (lambda () 123)) '())   ; --> 123

(my-eval
  '(((lambda (f) (f f))
     (lambda (f)
       ((lambda (le)
          (lambda (l)
            (cond ((null? l) 0)
                  (else (+ 1 (le (cdr l)))))))
        (lambda (x) ((f f) x)))))
    '(a b c d e f g))
  '())                                                    ; --> 7
