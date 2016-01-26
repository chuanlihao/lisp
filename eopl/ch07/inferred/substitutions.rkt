#lang racket

(require eopl "lang.rkt")
(provide (all-defined-out))

; apply-one-subst : Type * Tvar * Type -> Type (ty0[tvar = ty1])
(define apply-one-subst
  (lambda (ty0 tvar ty1)
    (cases type ty0
      (int-type () ty0)
      (bool-type () ty0)
      (proc-type (arg-type result-type)
        (proc-type (apply-one-subst arg-type tvar ty1)
                   (apply-one-subst result-type tvar ty1)))
      (tvar-type (sn)
        (if (equal? ty0 tvar) ty1 ty0)))))

; apply-subst-to-type : Type * Subst -> Type
(define apply-subst-to-type
  (lambda (ty subst)
    (cases type ty
      (int-type () ty)
      (bool-type () ty)
      (proc-type (t1 t2)
        (proc-type (apply-subst-to-type t1 subst)
                   (apply-subst-to-type t2 subst)))
      (tvar-type (sn)
        (let ((tmp (assoc ty subst)))
          (if tmp (cdr tmp) ty))))))

; empty-subst : () -> Subst
(define empty-subst
  (lambda ()
    '()))

; extend-subst : Subst * Tvar * Type -> Subst
(define extend-subst
  (lambda (subst tvar ty)
    (cons (cons tvar ty)
          (map (lambda (p)
                 (cons (car p)
                       (apply-one-subst (cdr p) tvar ty)))
               subst))))

(define substitution? list?)