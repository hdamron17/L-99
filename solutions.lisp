;;;; Copyright 2019 Hunter Damron

(defun my-last (l)
  (let ((this-cdr (cdr l)))
    (if (null this-cdr)
      (car l)
      (my-last this-cdr))))

(defun ~my-but-last (l prev)
  (if (null (cdr l))
    (car prev)
    (~my-but-last (cdr l) l)))
(defun my-but-last (l) (~my-but-last l nil))

(defun element-at (l k)
  (if (<= k 0)
    (car l)
    (element-at (cdr l) (- k 1))))
