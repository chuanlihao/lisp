#lang racket

(require eopl "lang.rkt" "static-data-structures.rkt" "subtyping.rkt" "checker.rkt")
(provide (all-defined-out))

(define defns-to-decls
  (lambda (defns tenv)
    (if (null? defns)
        '()
        (cases definition (car defns)
          (val-defn (var-name exp)
            (let ((ty (type-of exp tenv)))
              (cons
                (var-decl var-name ty)
                (defns-to-decls (cdr defns) (extend-tenv var-name ty tenv)))))))))

(define interface-of
  (lambda (m-body tenv)
    (cases module-body m-body
      (defns-module-body (defns)
        (simple-iface
          (defns-to-decls defns tenv))))))

(define add-module-defns-to-tenv
  (lambda (defns tenv)
    (if (null? defns)
        tenv
        (cases module-definition (car defns)
          (a-module-definition (m-name expected-iface m-body)
            (let ((actual-iface (interface-of m-body tenv)))
              (if (<:-iface actual-iface expected-iface)
                  (add-module-defns-to-tenv
                    (cdr defns)
                    (extend-tenv-with-module m-name expected-iface tenv))
                  (error 'add-module-defns-to-tenv-unsatisfied-iface))))))))

(define type-of-program
  (lambda (prog)
    (cases program prog
      (a-program (module-defs body)
        (type-of body
                 (add-module-defns-to-tenv module-defs (empty-tenv)))))))
