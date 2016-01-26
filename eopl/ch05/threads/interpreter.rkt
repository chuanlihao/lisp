#lang racket

(require eopl "../../env.rkt" "lang.rkt" "datatypes.rkt" "store.rkt" "scheduler.rkt" "semaphore.rkt")
(provide (all-defined-out))

(define value-of-program
  (lambda (timeslice pgm)
    (initialize-store!)
    (initialize-scheduler! timeslice)
    (cases program pgm
      (a-program (exp)
                 (value-of/k exp (empty-env) (end-main-thread-cont))))))

(define value-of/k
  (lambda (exp env cont)
    (cases expression exp
      (const-exp (num)
                 (apply-cont cont (num-val num)))
      (var-exp (var)
               (begin
                 (apply-cont cont (deref (apply-env env var)))))
      (diff-exp (exp1 exp2)
                (value-of/k exp1 env (diff-cont-1 exp2 env cont)))
      (zero?-exp (exp)
                 (value-of/k exp env (zero-cont cont)))
      (if-exp (exp1 exp2 exp3)
              (value-of/k exp1 env (if-cont exp2 exp3 env cont)))
      (let-exp (var exp body)
               (value-of/k exp env (let-cont var body env cont)))
      (proc-exp (var body)
                (apply-cont cont (proc-val (procedure var body env))))
      (call-exp (rator rand)
                (value-of/k rator env (rator-cont rand env cont)))
      (letrec-exp (p-name b-var p-body letrec-body)
                  (value-of/k letrec-body
                              (extend-env-rec p-name b-var p-body env)
                              cont))
      (assign-exp (var exp)
                  (value-of/k exp env (assign-cont var env cont)))
      (begin-exp (exp1 exps)
                 (apply-cont (begin-cont exp1 exps env cont) (num-val 42)))
      (print-exp (exp)
                 (value-of/k exp env (print-cont cont)))
      (spawn-exp (exp)
                 (value-of/k exp env (spawn-cont cont)))
      (mutex-exp ()
                 (apply-cont cont (mutex-val (new-mutex))))
      (wait-exp (exp)
                (value-of/k exp env (wait-cont cont)))
      (signal-exp (exp)
                  (value-of/k exp env (signal-cont cont)))
      (else (error 'unexpected-expression)))))

(define procedure
  (lambda (var body env)
    (lambda (val cont)
      (value-of/k body (extend-env var (newref val) env) cont))))

(define apply-procedure/k
  (lambda (proc val cont)
    (proc val cont)))

(define extend-env-rec
  (lambda (p-name b-var p-body saved-env)
    (letrec ((new-env (lambda (var)
                        (if (eqv? var p-name)
                            p-ref
                            (apply-env saved-env var))))
             (p-ref (newref (proc-val (procedure b-var p-body new-env)))))
      new-env)))

(define apply-cont
  (lambda (current-cont val)
    (if (time-expired?)
        (begin
          (place-on-ready-queue!
            (lambda () (apply-cont current-cont val)))
          (run-next-thread))
        (begin
          (decrement-timer!)
          (cases continuation current-cont
            (zero-cont (cont)
              (apply-cont cont (bool-val (zero? (expval->num val)))))
            (if-cont (then-part else-part env cont)
              (value-of/k (if (expval->bool val)
                              then-part
                              else-part)
                          env
                          cont))
            (let-cont (var body env cont)
              (value-of/k body (extend-env var (newref val) env) cont))
            (diff-cont-1 (exp2 env cont)
              (value-of/k exp2 env (diff-cont-2 val cont)))
            (diff-cont-2 (val1 cont)
              (apply-cont cont (num-val (- (expval->num val1) (expval->num val)))))
            (rator-cont (rand env cont)
              (value-of/k rand env (rand-cont val cont)))
            (rand-cont (proc cont)
              (apply-procedure/k (expval->proc proc) val cont))
            (assign-cont (var env cont)
              (begin
                (setref! (apply-env env var) val)
                (apply-cont cont (num-val 27))))
            (begin-cont (exp1 exps env cont)
              (value-of/k exp1 env (if (null? exps)
                                       cont
                                       (begin-cont (car exps) (cdr exps) env cont))))
            (print-cont (cont)
              (begin
                (display (expval->num val))
                (newline)
                (apply-cont cont (num-val 43))))
            (spawn-cont (cont)
              (begin
                (place-on-ready-queue!
                  (lambda ()
                    (apply-procedure/k (expval->proc val)
                                       (num-val 28)
                                       (end-subthread-cont))))
                (apply-cont cont (num-val 73))))
            (end-main-thread-cont ()
              (begin
                (set-final-answer! val)
                (run-next-thread)))
            (end-subthread-cont ()
              (run-next-thread))
            (wait-cont (cont)
              (wait-for-mutex
                (expval->mutex val)
                (lambda () (apply-cont cont (num-val 52)))))
            (signal-cont (cont)
              (signal-mutex
                (expval->mutex val)
                (lambda () (apply-cont cont (num-val 53)))))
)))))
