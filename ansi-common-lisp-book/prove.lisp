; Prove-problem, like a small Prolog language.
; From book ANSI Common Lisp.

; binds has format '((?x x) (?y y))
(defun match (x y &optional binds)
  (cond
    ((eql x y) (values binds t))
    ((assoc x binds) (match (binding x binds) y binds))
    ((assoc y binds) (match x (binding y binds) binds))
    ((var? x) (values (cons (cons x y) binds) t))
    ((var? y) (values (cons (cons y x) binds) t))
    (t
      (when (and (consp x) (consp y))
        (multiple-value-bind (b2 yes?)
                             (match (car x) (car y) binds)
          (and yes? (match (cdr x) (cdr y) b2))))))) 

(defun binding (x binds)
  (let ((b (assoc x binds)))
    (if b
        (or (binding (cdr b) binds)
            (cdr b)))))

(defun var? (x)
  (and (symbolp x)
       (eql (char (symbol-name x) 0) #\?)))

; data structure
(defvar *rules* (make-hash-table))

(defun <-f (con &optional ant)
  (push (cons (cdr con) ant)
        (gethash (car con) *rules*)))

(defmacro <- (con &optional ant)
  `(<-f ',con ',ant))

; All prove-* functions get a binds and return a list of binds
(defun prove (expr &optional binds)
  (case (car expr)
    (and (prove-and (reverse (cdr expr)) binds))
    (or  (prove-or (cdr expr) binds))
    (not (prove-not (cadr expr) binds)) 
    (t   (prove-simple (car expr) (cdr expr) binds))))

(defun prove-and (clauses binds)
  (if (null clauses)
      (list binds)
      (mapcan #'(lambda (b)
                  (prove (car clauses) b))
              (prove-and (cdr clauses) binds))))

(defun prove-or (clauses binds)
  (mapcan #'(lambda (c) (prove c binds))
          clauses))

; sample for not: (<- (small-alpha ?x) (and (alpha ?x) (not (big ?x))))
(defun prove-not (clause binds)
  (unless (prove clause binds)
    (list binds)))

(defun prove-simple (pred args binds)
  (mapcan #'(lambda (r)
              (multiple-value-bind (b2 yes)
                                   (match args (car r) binds)
                (when yes
                  (if (cdr r)
                      (prove (cdr r) b2)
                      (list b2)))))
          (mapcar #'change-vars
                  (gethash pred *rules*))))

(defun change-vars (r)
  (sublis (mapcar #'(lambda (v) (cons v (gensym "?")))
                  (vars-in r))
          r))

(defun vars-in (expr)
  (if (atom expr)
      (if (var? expr) (list expr))
      (union (vars-in (car expr))
             (vars-in (cdr expr)))))

; A function to reduce the output
(defun prove-with-simple-output (statement)
  (mapcar #'(lambda (binds)
              (mapcar #'(lambda (v)
                          (cons v (binding v binds)))
                      (vars-in statement)))
          (prove statement)))

; Samples
(<- (son Bob Donald))
(<- (daught Alice Donald))
(<- (child ?x ?y) (or (son ?x ?y) (daught ?x ?y)))
(prove-with-simple-output '(child ?x ?y))

(setf *rules* (make-hash-table))
(<- (child Bob Donald))
(<- (child Alice Donald))
(<- (male Bob))
(<- (daught ?x ?y) (and (child ?x ?y) (not (male ?x))))
(prove-with-simple-output '(daught ?x ?y))
