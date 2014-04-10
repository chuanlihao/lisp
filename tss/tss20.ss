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


(define abort (lambda (x) x))


(define the-empty-table
  (lambda (name)
    (abort (build 'no-answer name))))

(define global-table the-empty-table)

(define extend
  (lambda (name1 value table)
    (lambda (name2)
      (if (eq? name2 name1)
          value
          (table name2)))))

(define multi-extend
  (lambda (names vals table)
    (if (null? names)
        table
        (multi-extend
          (cdr names)
          (cdr vals)
          (extend (car names) (car vals) table)))))

(define lookup
  (lambda (table name)
    (table name)))


(define box
  (lambda (it) 
    (lambda (sel)
      (sel it (lambda (new)
                (set! it new))))))

(define setbox
  (lambda (box new)
    (box (lambda (it set) (set new)))))

(define unbox
  (lambda (box)
    (box (lambda (it set) it))))

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
          ((eq? (car e) 'letcc) *letcc)
          ((eq? (car e) 'set!) *set!)
          ((eq? (car e) 'cond) *cond)
          (else *application))
        *application)))

(define value
  (lambda (e)
    (call/cc
      (lambda (the-end)
        (set! abort the-end)
        (cond
          ((define? e) (*define e))
          (else (the-meaning e)))))))

(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))

(define the-meaning
  (lambda (e)
    (meaning e lookup-in-global-table)))

(define lookup-in-global-table     ; the most recent global-table
  (lambda (name)
    (lookup global-table name)))

(define define?
  (lambda (e)
    (cond
      ((atom? e) #f)
      ((atom? (car e))
       (eq? (car e) 'define))
      (else #f))))

(define *define
  (let ((name-of second)
        (right-side-of third))
    (lambda (e)
      (set! global-table
            (extend
              (name-of e)
              (box (the-meaning
                     (right-side-of e)))
              global-table)))))

(define *set!
  (let ((name-of second)
        (right-side-of third))
    (lambda (e table)
      (setbox
        (lookup table (name-of e))
        (meaning (right-side-of e) table)))))

(define *quote
  (let ((text-of second))
    (lambda (e table)
      (text-of e))))

(define *letcc
  (let ((ccbody-of cddr)
        (name-of second))
    (lambda (e table)
      (call/cc
        (lambda (skip)
          (beglis (ccbody-of e)
                  (extend
                    (name-of e)
                    (box (a-prim skip))
                    table)))))))

(define *identifier
  (lambda (e table)
    (unbox (lookup table e))))

(define table-of first)
(define formals-of second)
(define body-of cddr)

(define *lambda
  (lambda (e table)
    (lambda (args)
      (beglis
        (body-of e)
        (multi-extend
          (formals-of e)
          (box-all args)
          table)))))

(define beglis
  (lambda (es table)
    (if (null? (cdr es))
        (meaning (car es) table)
        ((lambda (val)
           (beglis (cdr es) table))
         (meaning (car es) table)))))

(define box-all
  (lambda (args)
    (if (null? args)
        '()
        (cons (box (car args))
              (box-all (cdr args))))))

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
      ((meaning (function-of e) table)
       (evlis (arguments-of e) table)))))

(define a-prim
  (lambda (p)
    (lambda (args-in-a-list)
      (p (car args-in-a-list)))))

(define b-prim
  (lambda (p)
    (lambda (args-in-a-list)
      (p (first args-in-a-list)
         (second args-in-a-list)))))

(define *const
  (let ((:cons (b-prim cons))
        (:car (a-prim car))
        (:cdr (a-prim cdr))
        (:null? (a-prim null?))
        (:eq? (b-prim eq?))
        (:atom? (a-prim atom?))
        (:zero? (a-prim zero?))
        (:add1 (a-prim add1))
        (:sub1 (a-prim sub1))
        (:number? (a-prim number?)))
    (lambda (e table)
      (cond
        ((number? e) e)
        ((eq? e #t) #t)
        ((eq? e #f) #f)
        ((eq? e 'cons) :cons)
        ((eq? e 'car) :car)
        ((eq? e 'cdr) :cdr)
        ((eq? e 'eq?) :eq?)
        ((eq? e 'atom?) :atom?)
        ((eq? e 'null?) :null?)
        ((eq? e 'zero?) :zero?)
        ((eq? e 'add1) :add1)
        ((eq? e 'sub1) :sub1)
        ((eq? e 'number?) :number?)
        (else (abort (build 'invalid-const e)))))))
