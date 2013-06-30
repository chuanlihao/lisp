; progs from chapter 05, book On Lisp

(defvar *!equives* (make-hash-table))

(defun ! (fn)
  (or (gethash fn *!equives*) fn))

(defun def! (fn fn!)
  (setf (gethash fn *!equives*) fn!))


(defun memoize (fn)
  (let ((cache (make-hash-table :test #'equal)))
    #'(lambda (&rest args)
        (multiple-value-bind (val win) (gethash args cache)
          (if win
              val
              (setf (gethash args cache)
                    (apply fn args)))))))


(defun compose (&rest fns)
  (if fns
      (let ((fn1 (car (last fns)))
            (fns (butlast fns)))
        #'(lambda (&rest args)
            (reduce #'funcall
                    fns
                    :from-end t
                    :initial-value (apply fn1 args))))
      #'identity))


(defun fif (if then &optional else)
  #'(lambda (x)
      (if (funcall if x)
          (funcall then x)
          (if else (funcall else x)))))

(defun fint (fn &rest fns)
  (if (null fns)
      fn
      (let ((chain (apply #'fint fns)))
        #'(lambda (x)
            (and (funcall fn x) (funcall chain x))))))


(defun lrec (rec &optional base)
  (labels ((self (lst)
             (if (null lst)
                 (if (functionp base)
                     (funcall base)
                     base)
                 (funcall rec (car lst)
                              #'(lambda ()
                                  (self (cdr lst)))))))
    #'self))

; copy-list
(lrec #'(lambda (x f) (cons x (funcall f))))
; remove-duplicates
(lrec #'(lambda (x f) (adjoin x (funcall f))))
; find-if, for some function fn
(lrec #'(lambda (x f) (if (fn x) x (funcall f))))
; some, for some function fn
(lrec #'(lambda (x f) (or (fn x) (funcall f))))


(defun our-copy-tree (tree)
  (if (atom tree)
      tree
      (cons (our-copy-tree (car tree))
            (our-copy-tree (cdr tree)))))

(defun count-leaves (tree)
  (if (atom tree)
      (if tree 1 0)
      (+ (count-leaves (car tree))
         (count-leaves (cdr tree)))))

(defun flatten (tree)
  (if (atom tree)
      (mklist tree)
      (nconc (flatten (car tree))
             (flatten (cdr tree)))))

(defun mklist (obj)
  (if (listp obj)
      obj
      (list obj)))

(defun rfind-if (fn tree)
  (if (atom tree)
      (if tree
          (and (funcall fn tree) tree))
      (or (rfind-if fn (car tree))
          (rfind-if fn (cdr tree)))))

(defun ttrav (rec &optional (base #'identity))
  (labels ((self (tree)
                   (if (atom tree)
                       (if (functionp base)
                           (funcall base tree)
                           base)
                       (funcall rec 
                                (self (car tree))
                                (self (cdr tree))))))
    #'self))

; our-copy-tree
(ttrav #'cons)
; count-leaves
(ttrav #'+ #'(lambda (x) (if x 1 0)))
; flatten
(ttrav #'nconc #'mklist)


(defun trec (rec &optional (base #'identity))
  (labels
    ((self (tree)
       (if (atom tree)
           (if (functionp base)
               (funcall base tree)
               base)
           (funcall rec
                    tree
                    #'(lambda ()
                        (self (car tree)))
                    #'(lambda ()
                        (self (cdr tree)))))))
   #'self))

; rfind-if
(trec #'(lambda (o l r) (or (funcall l) (funcall r)))
      #'(lambda (leaf) (and (oddp leaf) leaf)))
