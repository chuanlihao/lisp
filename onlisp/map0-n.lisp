(defun map0-n (fn n)
  (labels ((inner (p)
             (if (<= p n)
                 (cons (funcall fn p) (inner (+ p 1))))))
    (inner 0)))
