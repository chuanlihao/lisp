#lang racket

(require eopl "lang.rkt")
(provide (all-defined-out))

(define <:-iface
  (lambda (iface1 iface2)
    (cases interface iface1
      (simple-iface (decls1)
        (cases interface iface2
          (simple-iface (decls2)
            (<:-decls decls1 decls2)))))))

(define <:-decls
  (lambda (ds1 ds2)
    (cond ((null? ds2) #t)
          ((null? ds1) #f)
          (else (let ((n1 (decl->name (car ds1)))
                      (n2 (decl->name (car ds2))))
                  (if (eqv? n1 n2)
                      (and (equal? (decl->type (car ds1))
                                   (decl->type (car ds2)))
                           (<:-decls (cdr ds1) (cdr ds2)))
                      (<:-decls (cdr ds1) ds2)))))))
