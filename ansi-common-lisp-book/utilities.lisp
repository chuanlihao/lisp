; From ANSI Common Lisp  chapter 6

(defun single? (lst)
  (and (consp lst) (null (car lst))))

(defun append1 (lst obj)
  (append lst (list obj)))

(defun map-int (fn n)
  (let ((acc nil))
    (dotimes (i n)
      (push (funcall fn i) acc))
    (nreverse acc)))

(defun filter (fn lst)
  (remove-if #'(lambda (x)
                 (not (funcall fn x)))
             lst))

(defun most (fn > lst)
  (reduce #'(lambda (px py)
              (if (> (car px) (car py))
                  px
                  py))
          (mapcar #'cons (mapcar fn lst) lst)))
