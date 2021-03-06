(defmacro while (test &rest body)
  `(do ()
       ((not ,test))
     ,@body))

(defun quicksort (vec l r)
  (let ((i l)
        (j r)
        (p (svref vec (round (+ l r) 2))))
    (while (<= i j)
      (while (< (svref vec i) p) (incf i))
      (while (> (svref vec j) p) (decf j))
      (when (<= i j)
        (rotatef (svref vec i) (svref vec j))
        (incf i)
        (decf j)))
    (if (< l j) (quicksort vec l j))
    (if (< i r) (quicksort vec i r)))
  vec)
