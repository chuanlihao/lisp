; A queue implemented by procedures
(define empty-queue
  (lambda ()
    (lambda (f)
      (f 'empty 'error))))

(define empty-queue?
  (lambda (q)
    (q (lambda (bq v)
         (symbol? bq)))))

(define insert
  (lambda (q v)
    (lambda (f)
      (f q v))))

(define delete
  (lambda (q)
    (q (lambda (bq v)
         (if (empty-queue? bq)
             (empty-queue)
             (insert (delete bq) v))))))

(define front
  (lambda (q)
    (q (lambda (bq v)
         (if (empty-queue? bq)
             v
             (front bq))))))

; usage
(define q (insert
            (insert
              (insert
                (insert
                  (insert
                    (empty-queue)
                    'a)
                  'b)
                'c)
              'd)
            'e))

(define queue-to-list
  (lambda (q)
    (if (empty-queue? q)
        '()
        (cons (front q)
              (queue-to-list (delete q))))))

(queue-to-list q)    ; (a b c d e)
