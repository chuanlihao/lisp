#lang racket

;; in-S? :: N -> Bool
(define in-S?
  (lambda (n)
    (if (zero? n)
        #t
        (if (>= (- n 3) 0)
            (in-S? (- n 3))
            #f))))

;; list-length :: List -> Int
(define list-length
  (lambda (lst)
    (if (null? lst)
        0
        (+ 1 (list-length (cdr lst))))))

;; nth-element :: List * Int -> SchemeVal
;; Index starts from zero.
(define nth-element
  (lambda (lst n)
    (if (null? lst)
        (report-list-too-short n)
        (if (zero? n)
            (car lst)
            (nth-element (cdr lst) (- n 1))))))

(define report-list-too-short
  (lambda (n)
    (error 'nth-element
           "List too short by ~s elements.~%" (+ n 1))))

;; remove-first :: Sym * Listof(Sym) -> Listof(Sym)
(define remove-first
  (lambda (s los)
    (if (null? los)
        '()
        (if (eqv? (car los) s)
            (cdr los)
            (cons (car los) (remove-first s (cdr los)))))))

;; Sym * LcExp -> Bool
(define occurs-free?
  (lambda (s exp)
    (cond ((symbol? exp) (eqv? exp s))
          ((eqv? (car exp) 'lambda)
           (and (not (eqv? (caadr exp) s))
                (occurs-free? s (caddr exp))))
          (else (or (occurs-free? s (car exp))
                    (occurs-free? s (cadr exp)))))))

;; Sym * Sym * S-list -> S-list
(define subst
  (lambda (new old slist)
    (if (null? slist)
        '()
        (cons (subst-in-s-exp new old (car slist))
              (subst new old (cdr slist))))))

;; Sym * Sym * S-exp -> S-exp
(define subst-in-s-exp
  (lambda (new old sexp)
    (if (symbol? sexp)
        (if (eqv? old sexp)
            new
            sexp)
        (subst new old sexp))))

;; Listof(SchemeVal) * Int -> Listof(List(Int, SchemeVal))
(define number-elements-from
  (lambda (lst n)
    (if (null? lst)
        '()
        (cons (cons n (car lst))
              (number-elements-from (cdr lst) (+ n 1))))))

;; Listof(SchemeVal) -> Listof(List(Int, SchemeVal))
(define number-elements
  (lambda (lst)
    (number-elements lst 0)))

;; Listof(Int) -> Int
(define list-sum
  (lambda (loi)
    (if (null? loi)
        0
        (+ (car loi)
           (list-sum (cdr loi))))))

;; Vectorof(Int) * Int -> Int
(define partial-vector-sum
  (lambda (v n)
    (if (negative? n)
        0
        (+ (vector-ref v n)
           (partial-vector-sum v (- n 1))))))

;; Vectorof(Int) -> Int
(define vector-sum
  (lambda (v)
    (partial-vector-sum v (- (vector-length v) 1))))

;; number-elements-v2 :: Listof(SchemeVal) -> Listof(List(Int, SchemeVal))
(define number-elements-v2
  (lambda (lst)
    (if (null? lst)
        '()
        (fun-g (list 0 (car lst)) (number-elements-v2 (cdr lst))))))

(define fun-g
  (lambda (first lst)
    (cons first
          (map (lambda (vals)
                 (cons (+ (car vals) 1)
                       (cdr vals)))
               lst))))