; progs from chapter 11, book On Lisp

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

; mvdo*
(defmacro mvdo* (parm-cl test-cl &body body)
  (mvdo-gen parm-cl parm-cl test-cl body))

(defun mvdo-gen (binds rebinds test body)
  (if (null binds)
      (let ((label (gensym)))
        `(prog nil
           ,label
           (if ,(car test)
               (return (progn ,@(cdr test))))
           ,@body
           ,@(mvdo-rebind-gen rebinds)
           (go ,label)))
      (let ((rec (mvdo-gen (cdr binds) rebinds test body)))
        (let ((var/s (caar binds)) (expr (cadar binds)))
          (if (atom var/s)
              `(let ((,var/s ,expr)) ,rec)
              `(multiple-value-bind ,var/s ,expr ,rec))))))

(defun mvdo-rebind-gen (rebinds)
  (cond ((null rebinds) nil)
        ((< (length (car rebinds)) 3)
         (mvdo-rebind-gen (cdr rebinds)))
        (t
         (cons (list (if (atom (caar rebinds))
                         `setq
                         `multiple-value-setq)
                     (caar rebinds)
                     (third (car rebinds)))
               (mvdo-rebind-gen (cdr rebinds))))))

; mvdo
(defun group (source n)
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
             (let ((rest (nthcdr n source)))
               (if (consp rest)
                   (rec rest (cons (subseq source 0 n) acc))
                   (nreverse (cons source acc))))))
    (if source (rec source nil) nil)))

(defun mklist (obj)
  (if (listp obj)
      obj
      (list obj)))

(defun shuffle (x y)
  (mapcan #'list x y))

(defmacro mvpsetq (&rest args)
  (let* ((pairs (group args 2))
         (syms  (mapcar #'(lambda (p)
                            (mapcar #'(lambda (x) (gensym))
                                   (mklist (car p))))
                        pairs)))
    (labels ((rec (ps ss)
               (if (null ps)
                   `(setq
                      ,@(mapcan #'(lambda (p s)
                                    (shuffle (mklist (car p))
                                             s))
                                pairs
                                syms))
                   (let ((body (rec (cdr ps) (cdr ss))))
                     (let ((var/s (caar ps))
                           (expr (cadar ps)))
                       (if (consp var/s)
                           `(multiple-value-bind ,(car ss)
                                                 ,expr
                              ,body)
                           `(let ((,@(car ss) ,expr))
                              ,body)))))))
      (rec pairs syms))))

(defmacro mvdo (binds (test &rest result) &body body)
  (let ((label (gensym)))
    `(prog nil
       (mvpsetq ,@(mapcan #'(lambda (b)
                              (list (car b) (cadr b)))
                          binds))
       ,label
       (if ,test
           (return (progn ,@result)))
       ,@body
       (mvpsetq ,@(mapcan #'(lambda (b)
                              (if (third b)
                                  (list (car b)
                                        (third b))))
                          binds))
       (go ,label))))
