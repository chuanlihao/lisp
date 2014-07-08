(defun schedule-first-pair (l)
asssooo
ooo
  (defun remove-first (e l)
    (cond ((null l) nil)
          ((eq e (car l)) (cdr l))
          (t (cons (car l) (remove-first e (cdr l))))))
  (defun schedule-first (l)
    (mapcar #'(lambda (e)
                (cons e (remove-first e l)))
            (remove-duplicates l)))
  (remove-if-not
    #'(lambda (x)
        (<= (eval (car x)) (eval (cadr x))))
    (mapcan #'(lambda (l) 
                (mapcar #'(lambda (lt)
                            (cons (car l) lt))
                        (schedule-first (cdr l))))
            (schedule-first l))))

(defun apply-operator (l)
  (let ((x (car l))
        (y (cadr l))
        (others (cddr l)))
    (append (mapcar #'(lambda (op)
                        (cons (list op x y) others))
                    (if (zerop (eval y))
                        '(+ - *)
                        '(+ - * /)))
            (mapcar #'(lambda (op)
                        (cons (list op y x) others))
                    (if (zerop (eval x)) '(-) '(- /))))))

(defun calculate (existing-tuples)
  (if (= (length (car existing-tuples)) 1)
         (remove-if-not #'(lambda (x) (= (eval x) 24))
                        (mapcar #'car existing-tuples))
         (calculate (mapcan #'(lambda (cal-unit) 
                                   (mapcan #'apply-operator
                                           (schedule-first-pair cal-unit)))
                               existing-tuples))))

(defun calculate-24 (numbers)
  (calculate (list numbers)))

; example
; (calculate-24 '(1 1 1 8))  --> ((* (+ 1 (+ 1 1)) 8))
