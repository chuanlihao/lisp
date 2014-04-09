;(define kons cons)
;(define kar car)
;(define kdr cdr)
;(define set-kdr! set-cdr!)
(define call/cc call-with-current-continuation)

(define bons
  (lambda (a)
    (let ((d '()))
      (lambda (selector)
        (selector
          (lambda (x) (set! d x))
          a
          d)))))

(define set-kdr!
  (lambda (c new-d)
    (c (lambda (s a d) (s new-d)))))

(define kons
  (lambda (a d)
    (let ((c (bons a)))
      (set-kdr! c d)
      c)))

(define kar
  (lambda (c)
    (c (lambda (s a d) a))))

(define kdr
  (lambda (c)
    (c (lambda (s a d) d))))


(define lots
  (lambda (m)
    (if (zero? m)
        '()
        (kons 'egg (lots (- m 1))))))

(define lenkth
  (lambda (l)
    (if (null? l)
        0
        (+ 1 (lenkth (kdr l))))))

(define add-at-end
  (lambda (l)
    (if (null? l)
        (kons 'egg '())
        (kons (kar l) (add-at-end (kdr l))))))

(define add-at-end-too
  (lambda (l)
    (letrec
        ((A (lambda (ls)
              (if (null? (kdr ls))
                  (set-kdr! ls (kons 'egg '()))
                  (A (kdr ls))))))
      (A l)
      l)))

(define add-at-end-2
  (letrec
      ((A (lambda (ls)
            (if (null? (kdr ls))
                (set-kdr! ls (kons 'egg '()))
                (A (kdr ls))))))
    (lambda (l)
      (A l)
      l)))

(define add-at-end-3
  (lambda (l)
    (call/cc
      (lambda (hop)
        (letrec
            ((A (lambda (ls)
                  (if (null? (kdr ls))
                      (begin
                        (set-kdr! ls (kons 'egg '()))
                        (hop '()))
                      (A (kdr ls))))))
          (A l))))
    l))

(define eklist?
  (lambda (ls1 ls2)
    (cond
      ((null? ls1) (null? ls2))
      ((null? ls2) #f)
      (else 
        (and (eq? (kar ls1) (kar ls2))
             (eklist? (kdr ls1) (kdr ls2)))))))

(define same?
  (lambda (c1 c2)
    (let ((t1 (kdr c1))
          (t2 (kdr c2)))
      (set-kdr! c1 1)
      (set-kdr! c2 2)
      (let ((v (= (kdr c1) (kdr c2))))
        (set-kdr! c1 t1)
        (set-kdr! c2 t2)
        v))))

(define last-kons
  (lambda (ls)
    (if (null? (kdr ls))
        ls
        (last-kons (kdr ls)))))

(define finate-lenkth
  (lambda (p)
    (call/cc
      (lambda (infinite)
        (letrec
            ((C (lambda (p q)
                  (cond
                    ((null? q) 0)
                    ((null? (kdr q)) 1)
                    ((same? p q)
                     (infinite #f))
                    (else
                      (+ 2 (C (kdr p)
                              (kdr (kdr q)))))))))
          (if (null? p)
              0
              (+ 1 (C p (kdr p)))))))))
