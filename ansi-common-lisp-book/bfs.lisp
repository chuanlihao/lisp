; From book ANSI Common Lisp, chapter 3

(defun bfs (net queue dest)
  (if (null queue)
      nil
      (let* ((head (car queue))
             (e (car head)))
        (if (eq e dest)
            (reverse head)
            (bfs net
                 (append (cdr queue)
                         (mapcar
                           #'(lambda (x) (cons x head))
                           (cdr (assoc e net))))
                 dest)))))

(bfs '((a b c) (b c) (c d)) '((a)) 'd)
