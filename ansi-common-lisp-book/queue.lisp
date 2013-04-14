; from book ANSI Common Lisp, chapter 12

(defun make-queue () (cons nil nil))

(defun enqueue (obj q)
  (if (null (car q))
      (setf (car q) (setf (cdr q) (list obj)))
      (setf (cddr q) (list obj)
            (cdr q) (cddr q)))
  (car q))

(defun dequeue (q)
  (pop (car q)))
