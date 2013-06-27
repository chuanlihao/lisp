; programs from chapter 02, On Lisp

(defun our-remove-if (fn lst)
  (if (null lst)
      nil
      (if (funcall fn (car lst))
          (our-remove-if fn (cdr lst))
          (cons (car lst) (our-remove-if fn (cdr lst))))))


(proclaim '(optimize speed))

(defun add-n (n)
  (labels ((tri (c n)
             (declare (type fixnum n c))
             (if (zerop n)
                 c
                 (tri (the fixnum (1+ c))
                      (the fixnum (- n 1))))))
    (tri 0 n)))

(compile 'add-n)
