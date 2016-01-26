#lang racket

(require eopl "lang.rkt")
(provide (all-defined-out))

(define-datatype type-environment type-environment?
  (empty-tenv)
  (extend-tenv
    (bvar symbol?)
    (bval type?)
    (saved-tenv type-environment?))
  (extend-tenv-with-module
    (name symbol?)
    (interface interface?)
    (saved-tenv type-environment?)))

(define test-tenv
  (lambda ()
    (extend-tenv
      'a
      (qualified-type 'm1 'a)
      (extend-tenv
        'b
        (qualified-type 'm2 'b)
        (extend-tenv-with-module
          'm2
          (simple-iface (list (var-decl 'a (bool-type)) (var-decl 'b (int-type))))
          (extend-tenv-with-module
            'm1
            (simple-iface (list (var-decl 'a (int-type)) (var-decl 'b (bool-type))))
            (empty-tenv)))))))

(define lookup-qualified-var-in-tenv
  (lambda (m-name var-name tenv)
    (let ((iface (lookup-module-name-in-tenv tenv m-name)))
      (cases interface iface
        (simple-iface (decls)
          (lookup-variable-name-in-decls var-name decls))))))

(define lookup-variable-name-in-tenv
  (lambda (tenv search-sym)
    (let ((maybe-answer (variable-name->maybe-binding-in-tenv tenv search-sym)))
      (if maybe-answer
          maybe-answer
          (error 'lookup-variable-name-in-tenv-failure)))))

(define apply-tenv lookup-variable-name-in-tenv)

(define lookup-module-name-in-tenv
  (lambda (tenv search-sym)
    (let ((maybe-answer (module-name->maybe-binding-in-tenv tenv search-sym)))
      (if maybe-answer
          maybe-answer
          (error 'lookup-module-name-in-tenv-failure)))))

(define lookup-variable-name-in-decls
  (lambda (var-name decls)
    (let loop ((decls decls))
      (cond
        ((null? decls) (error 'lookup-variable-name-in-decls-failure))
        ((eqv? var-name (decl->name (car decls))) (decl->type (car decls)))
        (else (loop (cdr decls)))))))

(define variable-name->maybe-binding-in-tenv
  (lambda (tenv search-sym)
    (let recur ((tenv tenv))
      (cases type-environment tenv
        (empty-tenv () #f)
        (extend-tenv (name ty saved-tenv)
          (if (eqv? name search-sym)
              ty
              (recur saved-tenv)))
        (else (recur (tenv->saved-tenv tenv)))))))

(define module-name->maybe-binding-in-tenv
  (lambda (tenv search-sym)
    (let recur ((tenv tenv))
      (cases type-environment tenv
        (empty-tenv () #f)
        (extend-tenv-with-module (name m-type saved-tenv)
          (if (eqv? name search-sym)
              m-type
              (recur saved-tenv)))
        (else (recur (tenv->saved-tenv tenv)))))))

(define tenv->saved-tenv
  (lambda (tenv)
    (cases type-environment tenv
      (empty-tenv () (error 'tenv->saved-tenv-failure))
      (extend-tenv (name ty saved-tenv) saved-tenv)
      (extend-tenv-with-module (name m-type saved-tenv) saved-tenv))))
