;;;; Copyright 2019 Hunter Damron

(load "solutions.lisp")

(defun problem (num f params correct)
  (let* ((ans (apply f params))
         (comment (if (= ans correct) "" (format nil " (Correct answer ~A)" correct))))
    (format t "Problem ~D:~%  ~A -> ~A~A~%" num params ans comment)))

(defun main ()
  (problem 1 'my-last '((3 1 4 9)) 9)
  (problem 2 'my-but-last '((3 1 4 9)) 4)
  (problem 3 'element-at '((3 1 4 8 7 2 13) 3) 8))
