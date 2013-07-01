; programs from chapter 07, book On Lisp

(defmacro me1 (expr)
  `(macroexpand-1 ',expr))

(defmacro nil! (var)
  (list 'setf var nil))

(defmacro nif (expr pos zero neg)
  `(case (truncate (signum ,expr))
     (1 ,pos)
     (0 ,zero)
     (-1 ,neg)))

(defmacro our-when (test &body body)
  `(if ,test
       (progn
         ,@body)))

(defmacro meq (obj lst)
  `(member ,obj ,lst :test #'eq))

(defmacro while (test &body body)
  `(do ()
       ((not ,test))
     ,@body))
