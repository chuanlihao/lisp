; From book ANSI Common Lisp, chapter 4

(defun bin-search (obj vec)
  (let ((len (length vec)))
    (and (not (zerop len))
         (finder obj vec 0 (- len 1)))))

(defun finder (obj vec start end)
  (and (<= start end)
       (let* ((len (- end start))
              (mid (+ start (round (/ len 2))))
              (test-obj (aref vec mid)))
         (cond ((< obj test-obj) (finder obj vec start (- mid 1)))
               ((> obj test-obj) (finder obj vec (+ mid 1) end))
               (t test-obj)))))

(bin-search 3 #(1 2 3 4 5 6 7 8 9))
