;;;; Copyright 2019 Hunter Damron

(defun my-last (l)
  "P01 Find the last box of a list"
  (reduce (lambda (a b) (declare (ignore a)) b) l))

(defun ~my-but-last (l prev)
  (if (null (cdr l))
    (car prev)
    (~my-but-last (cdr l) l)))
(defun my-but-last (l)
  "P02 Find the last but one box of a list"
  (~my-but-last l nil))

(defun element-at (l k)
  "P03 Find the K'th element of a list"
  (if (<= k 0)
    (car l)
    (element-at (cdr l) (- k 1))))

(defun my-len (l)
  "P04 Find the number of elements of a list"
  (reduce (lambda (a b) (declare (ignore b)) (+ a 1)) l :initial-value 0))

(defun my-rev (l)
  "P05 Reverse a list"
  (reduce (lambda (a b) (cons b a)) l :initial-value '()))

(defun palindrome (l)
  "P06 Find out whether a list is a palindrome"
  (every '= l (my-rev l)))
