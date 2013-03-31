; From ANSI Common Lisp  chapter 4

(defstruct (node (:print-function
                  (lambda (n s d)
                    (format s "#<~A>" (node-elt n)))))
  elt (l nil) (r nil))

(defun bst-insert (obj bst <)
  (if (null bst)
      (make-node :elt obj)
      (let ((elt (node-elt bst)))
        (cond ((funcall < obj elt)
               (make-node
                 :elt elt
                 :l (bst-insert obj (node-l bst) <)
                 :r (node-r bst)))
              ((funcall < elt obj)
               (make-node
                 :elt elt
                 :l (node-l bst)
                 :r (bst-insert obj (node-r bst) <)))
              (t bst)))))

(defun bst-find (obj bst <)
  (if (null bst)
      nil
      (let ((elt (node-elt bst)))
        (cond ((funcall < obj elt)
               (bst-find obj (node-l bst) <))
              ((funcall < elt obj)
               (bst-find obj (node-r bst) <))
              (t bst)))))

(defun in-order-visit (bst)
  (when bst
    (in-order-visit (node-l bst))
    (format t " ~A" (node-elt bst))
    (in-order-visit (node-r bst))))

(defun bst-min (bst)
  (and bst
       (or (bst-min (node-l bst)) bst)))

(defun bst-max (bst)
  (and bst
       (or (bst-max (node-r bst)) bst)))

(defun bst-remove (obj bst <)
  (if (null bst)
      nil
      (let ((elt (node-elt bst)))
        (cond ((funcall < obj elt)
               (make-node
                 :elt elt
                 :l (bst-remove obj (node-l bst) <)
                 :r (node-r bst)))
              ((funcall < elt obj)
               (make-node
                 :elt elt
                 :l (node-l bst)
                 :r (bst-remove obj (node-r bst) <)))
              (t (precolate bst))))))

(defun precolate (bst)
  (let ((l (node-l bst))
        (r (node-r bst)))
    (cond ((null l) r)
          ((null r) l)
          (t (if (zerop (random 2))
                 (make-node
                   :elt (node-elt (bst-max l))
                   :l (bst-remove-max l)
                   :r r)
                 (make-node
                   :elt (node-elt (bst-min r))
                   :l l
                   :r (bst-remove-min r)))))))

(defun bst-remove-max (bst)
  (if (null (node-r bst))
      (node-l bst)
      (make-node
        :elt (node-elt bst)
        :l (node-l bst)
        :r (bst-remove-max (node-r bst)))))

(defun bst-remove-min (bst)
  (if (null (node-l bst))
      (node-r bst)
      (make-node 
        :elt (node-elt bst)
        :l (bst-remove-min (node-l bst))
        :r (node-r bst))))

(let ((nums nil))
  (dolist (x '(5 8 4 2 1 9 6 7 3))
    (setf nums (bst-insert x nums #'<)))
  (in-order-visit nums)
  (format t "~%")
  (setf nums (bst-remove 5 nums #'<))
  (format t "~%")
  (in-order-visit nums))
