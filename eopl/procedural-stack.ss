; A stack implemented by procedures
(define empty-stack
  (lambda ()
    (lambda (f)
      (f 'error 'empty))))

(define empty-stack?
  (lambda (s)
    (s (lambda (v bs)
         (symbol? bs)))))

(define push
  (lambda (v s)
    (lambda (f)
      (f v s))))

(define pop
  (lambda (s)
    (s (lambda (v bs) bs))))

(define top
  (lambda (s)
    (s (lambda (v bs) v))))

; usage
(define s (push 'a
            (push 'b
              (push 'c
                (push 'd
                  (empty-stack))))))

(define stack-to-list
  (lambda (s)
    (if (empty-stack? s)
        '()
        (cons (top s)
              (stack-to-list (pop s))))))

(stack-to-list s)           ; (a b c d)
