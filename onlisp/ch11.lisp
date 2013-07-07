(defmacro our-let (binds &body body)
  `((lambda ,(mapcar #'(lambda (x)
                         (if (consp x) (car x) x))
                     binds)
      ,@body)
    ,@(mapcar #'(lambda (x)
                  (if (consp x) (cadr x) nil))
              binds)))

(defmacro our-let* (binds &body body)
  `(our-let ,(if (consp binds) `(,(car binds)) nil)
      ,(if (null binds)
           `(progn ,@body)
           `(our-let* ,(cdr binds) ,@body))))


(defmacro when-bind ((var expr) &body body)
  `(let ((,var ,expr))
     (when ,var
       ,@body)))

(defmacro when-bind* (binds &body body)
  (if (null binds)
      `(progn ,@body)
      `(when-bind ,(car binds)
         (when-bind* ,(cdr binds) ,@body))))

(defmacro our-with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s) `(,s (gensym)))
                 syms)
     ,@body))


; condition-let
(defmacro condlet (clauses &body body)
  (let ((bodfn (gensym))
        (vars (mapcar #'(lambda (v) (cons v (gensym)))
                      (remove-duplicates
                        (mapcar #'car
                          (mapcan #'(lambda (x) (copy-list (cdr x))) 
                                  clauses))))))
    `(labels ((,bodfn ,(mapcar #'car vars)
                  ,@body))
       (cond ,@(mapcar #'(lambda (cl)
                           (condlet-clause vars cl bodfn))
                       clauses)))))

(defun condlet-clause (vars cl bodfn)
  `(,(car cl) (let ,(mapcar #'cdr vars)
                (let ,(condlet-binds vars (cdr cl))
                  (,bodfn ,@(mapcar #'cdr vars))))))

(defun condlet-binds (vars cl)
  (mapcar #'(lambda (bindform)
              (if (consp bindform)
                  (list (cdr (assoc (car bindform) vars))
                        (cadr bindform))))
          cl))


; with- macro
(setf *db* 'db)
(defun lock (db) (format t "LOCK ~A~%" db))
(defun release (db) (format t "RELEASE ~A~%" db))

(defmacro with-db (db &body body)
  (let ((temp (gensym)))
    `(let ((,temp *db*))
       (unwind-protect
         (progn
           (setq *db* ,db)
           (lock *db*)
           ,@body)
         (progn
           (release *db*)
           (setq *db* ,temp))))))

(defmacro with-db-fun (db &body body)
  (let ((gbod (gensym)))
    `(let ((,gbod #'(lambda () ,@body)))
       (declare (dynamic-extend ,gbod))
       (with-db-fn *db* ,db ,gbod))))

(defun with-db-fn (old-db new-db body)
  (unwind-protect
    (progn
      (setq *db* new-db)
      (lock *db*)
      (funcall body))
    (progn
      (release *db*)
      (setq *db* old-db))))


; conditional evaluation
(defmacro if3 (test t-case nil-case ?-case)
  `(case ,test
     ((nil) ,nil-case)
     (?     ,?-case)
     (t     ,t-case)))

(defmacro nif (expr pos zero neg)
  (let ((g (gensym)))
    `(let ((,g ,expr))
       (cond ((plusp ,g) ,pos)
             ((zerop ,g) ,zero)
             (t          ,neg)))))


(defmacro in (obj &rest choices)
  (let ((insym (gensym)))
    `(let ((,insym ,obj))
       (or ,@(mapcar #'(lambda (c) `(eql ,insym ,c))
                     choices)))))

(defmacro inq (obj &rest args)
  `(in ,obj ,@(mapcar #'(lambda (a)
                          `',a)
                      args)))

(defmacro in-if (fn &rest choices)
  (let ((fnsym (gensym)))
    `(let ((,fnsym ,fn))
       (or ,@(mapcar #'(lambda (c)
                         `(funcall ,fnsym ,c))
                     choices)))))

(defmacro >case (expr &rest clauses)
  (let ((g (gensym)))
    `(let ((,g ,expr))
       (cond ,@(mapcar #'(lambda (cl) (>casex g cl))
                       clauses)))))

(defun >casex (g cl)
  (let ((key (car cl)) (rest (cdr cl)))
    (cond ((consp key) `((in ,g ,@key) ,@rest))
          ((inq key t otherwise) `(t ,@rest))
          (t (error "bad >case clause")))))


; loops
(defmacro forever (&body body)
  `(do ()
       (nil)
     ,@body))

(defmacro while (test &body body)
  `(do ()
       ((not ,test))
     ,@body))

(defmacro till (test &body body)
  `(do ()
       (,test)
     ,@body))

(defmacro for ((var start stop) &body body)
  (let ((gstop (gensym)))
    `(do ((,var ,start (1+ ,var))
          (,gstop ,stop))
         ((> ,var ,gstop))
       ,@body)))


(defun map0-n (fn n)
  (labels ((rec (i)
             (if (<= i n)
                 (cons (funcall fn i)
                       (rec (1+ i))))))
    (rec 0)))

; sample: (do-tuples/o (x y) '(a b c d) (princ (list x y)))
(defmacro do-tuples/o (params source &body body)
  (let ((src (gensym)))
    `(let ((,src ,source))
       (mapc #'(lambda ,params ,@body)
             ,@(map0-n #'(lambda (n)
                           `(nthcdr ,n ,src))
                       (1- (length params)))))))

(defun map1-n (fn n)
  (labels ((rec (i)
             (if (<= i n)
                 (cons (funcall fn i)
                       (rec (1+ i))))))
    (rec 1)))

; sample: (do-tuples/c (x y) '(a b c d) (princ (list x y)))
(defmacro do-tuples/c (params source &body body)
  (let ((src (gensym)) (dlst (gensym)))
    `(let* ((,src ,source)
            (,dlst (append ,src ,src)))
       (mapc #'(lambda ,params ,@body)
             ,src
             ,@(map1-n #'(lambda (n)
                           `(nthcdr ,n ,dlst))
                       (1- (length params)))))))
