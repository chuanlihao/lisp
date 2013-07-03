; progs from chapter 08, book On Lisp

; definition for object
(defstruct obj
  x y)

; util functions
(defun bounds (objs)
  (let ((obj-xs (mapcar #'(lambda (o) (obj-x o)) objs))
        (obj-ys (mapcar #'(lambda (o) (obj-y o)) objs)))
    (values (apply #'min obj-xs)
            (apply #'min obj-ys)
            (apply #'max obj-xs)
            (apply #'max obj-ys))))

(defun redraw (xmin ymin xmax ymax)
  (format t "Redraw: ~A ~A ~A ~A~%" xmin ymin xmax ymax))

; original implementations
(defun move-objs-1 (objs dx dy)
  (multiple-value-bind (x0 y0 x1 y1) (bounds objs)
    (dolist (o objs)
      (incf (obj-x o) dx)
      (incf (obj-y o) dy))
    (multiple-value-bind (xa ya xb yb) (bounds objs)
      (redraw (min x0 xa) (min y0 ya)
              (max x1 xb) (max y1 yb)))))

(defun scale-objs-1 (objs factor)
  (multiple-value-bind (x0 y0 x1 y1) (bounds objs)
    (dolist (o objs)
      (setf (obj-x o) (* (obj-x o) factor)
            (obj-y o) (* (obj-y o) factor)))
    (multiple-value-bind (xa ya xb yb) (bounds objs)
      (redraw (min x0 xa) (min y0 ya)
              (max x1 xb) (max y1 yb)))))

; implements with macro
(defmacro with-redraw ((var objs) &body body)
  (let ((gob (gensym))
        (x0 (gensym)) (y0 (gensym))
        (x1 (gensym)) (y1 (gensym)))
    `(let ((,gob ,objs))
       (multiple-value-bind (,x0 ,y0 ,x1 ,y1) (bounds ,gob)
         (dolist (,var ,gob) ,@body)
         (multiple-value-bind (xa ya xb yb) (bounds ,gob)
           (redraw (min ,x0 xa) (min ,y0 ya)
                   (max ,x1 xb) (max ,y1 yb)))))))

(defun move-objs-2 (objs dx dy)
  (with-redraw (o objs)
    (incf (obj-x o) dx)
    (incf (obj-y o) dy)))

(defun scale-objs-2 (objs factor)
  (with-redraw (o objs)
    (setf (obj-x o) (* (obj-x o) factor)
          (obj-y o) (* (obj-y o) factor))))

; implements with common functions
(defun update-redraw (objs update-fn)
  (multiple-value-bind (x0 y0 x1 y1) (bounds objs)
    (dolist (o objs)
      (funcall update-fn o))
    (multiple-value-bind (xa ya xb yb) (bounds objs)
      (redraw (min x0 xa) (min y0 ya)
              (max x1 xb) (max y1 yb)))))

(defun move-objs-3 (objs dx dy)
  (update-redraw objs
                 #'(lambda (o)
                     (incf (obj-x o) dx)
                     (incf (obj-y o) dy))))

(defun scale-objs-3 (objs factor)
  (update-redraw objs
                 #'(lambda (o)
                     (setf (obj-x o) (* (obj-x o) factor)
                           (obj-y o) (* (obj-y o) factor)))))
