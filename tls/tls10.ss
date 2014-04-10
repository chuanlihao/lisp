(define call/cc call-with-current-continuation)
(define first car)
(define second cadr)
(define third caddr)
(define add1
  (lambda (x) (+ x 1)))
(define sub1
  (lambda (x) (- x 1)))
(define build
  (lambda (x y) (cons x (cons y '()))))
(define atom?
  (lambda (x)
    (and (not (null? x)) (not (pair? x)))))


(define the-empty-table
  (lambda (name)
    ...))

(define extend
  (lambda (name1 value table)
    (lambda (name2)
      (if (eq? name2 name1)
          value
          (table name2)))))

(define extend-entry
  (lambda (names vals table)
    (if (null? names)
        table
        (extend-entry
          (cdr names)
          (cdr vals)
          (extend (car names) (car vals) table)))))

(define lookup
  (lambda (table name)
    (table name)))

(define example-table
  (extend-entry
    '(x y)
    '(1 2)
    (extend 'x 3 the-empty-table)))


(define expression-to-action
  (lambda (e)
    (if (atom? e)
        (atom-to-action e)
        (list-to-action e))))

(define atom-to-action
  (lambda (e)
    (cond
      ((number? e) *const)
      ((eq? e #t) *const)
      ((eq? e #f) *const)
      ((eq? e 'cons) *const)
      ((eq? e 'car) *const)
      ((eq? e 'cdr) *const)
      ((eq? e 'null?) *const)
      ((eq? e 'eq?) *const)
      ((eq? e 'atom?) *const)
      ((eq? e 'zero?) *const)
      ((eq? e 'add1) *const)
      ((eq? e 'sub1) *const)
      ((eq? e 'number?) *const)
      (else *identifier))))

(define list-to-action
  (lambda (e)
    (if (atom? (car e))
        (cond
          ((eq? (car e) 'quote) *quote)
          ((eq? (car e) 'lambda) *lambda)
          ((eq? (car e) 'cond) *cond)
          (else *application))
        *application)))

(define value
  (lambda (e)
    (meaning e the-empty-table)))

(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))

(define *const
  (lambda (e table)
    (cond
      ((number? e) e)
      ((eq? e #t) #t)
      ((eq? e #f) #f)
      (else (build 'primitive e)))))   ; e.g., (primitive car)

(define *quote
  (let ((text-of second))
    (lambda (e table)
      (text-of e))))

(define *identifier
  (lambda (e table)
    (lookup table e)))

(define *lambda
  (lambda (e table)
    (build 'non-primitive
           (cons table (cdr e)))))

(define table-of first)
(define formals-of second)
(define body-of third)

(define evcon
  (let ((else? (lambda (x)
                 (and (atom? x) (eq? x 'else))))
        (question-of first)
        (answer-of second))
    (lambda (lines table)
      (cond
        ((else? (question-of (car lines)))
         (meaning (answer-of (car lines)) table))
        ((meaning (question-of (car lines)) table)
         (meaning (answer-of (car lines)) table))
        (else (evcon (cdr lines) table))))))

(define *cond
  (let ((cond-lines-of cdr))
    (lambda (e table)
      (evcon (cond-lines-of e) table))))

(define evlis
  (lambda (args table)
    (if (null? args)
        '()
        (cons (meaning (car args) table)
              (evlis (cdr args) table)))))

(define *application
  (let ((function-of car)
        (arguments-of cdr))
    (lambda (e table)
      (my-apply
        (meaning (function-of e) table)
        (evlis (arguments-of e) table)))))

(define my-apply
  (let ((primitive? (lambda (l)
                      (eq? (first l) 'primitive)))
        (non-primitive? (lambda (l)
                          (eq? (first l) 'non-primitive))))
    (lambda (fun vals)
      (cond
        ((primitive? fun)
         (apply-primitive (second fun) vals))
        ((non-primitive? fun)
         (apply-closure (second fun) vals))
        (else 'invalid-function)))))

(define apply-primitive
  (lambda (name vals)
    (cond
      ((eq? name 'cons)
       (cons (first vals) (second vals)))
      ((eq? name 'car)
       (car (first vals)))
      ((eq? name 'cdr)
       (cdr (first vals)))
      ((eq? name 'null?)
       (null? (first vals)))
      ((eq? name 'eq?)
       (eq? (first vals) (second vals)))
      ((eq? name 'atom?)
       (:atom? (first vals)))
      ((eq? name 'zero?)
       (zero? (first vals)))
      ((eq? name 'add1)
       (add1 (first vals)))
      ((eq? name 'sub1)
       (sub1 (first vals)))
      ((eq? name 'number?)
       (number? (first vals))))))

(define :atom?
  (lambda (x)
    (cond
      ((atom? x) #t)
      ((null? x) #f)
      ((eq? (car x) 'primitive) #t)     ; treat functions as atom
      ((eq? (car x) 'non-primitive) #t)
      (else #f))))

(define apply-closure
  (lambda (closure vals)
    (meaning
      (body-of closure)
      (extend-entry
        (formals-of closure)
        vals
        (table-of closure)))))
