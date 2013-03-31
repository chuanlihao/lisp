; From ANSI Commpn Lisp chapter 6

(defun compose (&rest fns)
  (destructuring-bind (fn1 . rest) (reverse fns)
    #'(lambda (&rest args)
        (reduce #'(lambda (v f) (funcall f v))
                rest
                :initial-value (apply fn1 args)))))

(defun disjoin (fn &rest fns)
  (if (null fns)
      fn
      #'(lambda (&rest args)
          (or (apply fn args)
              (apply (apply #'disjoin fns) args)))))

(defun conjoin (fn &rest fns)
  (if (null fns)
      fn
      #'(lambda (&rest args)
          (and (apply fn args)
               (apply (apply #'conjoin fns) args)))))

(defun curry (fn &rest args)
  #'(lambda (&rest args2)
      (apply fn (append args args2))))

(defun rcurry (fn &rest args)
  #'(lambda (&rest args2)
      (apply fn (append args2 args))))

(defun always (x)
  #'(lambda (&rest args) x))
