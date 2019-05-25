;;;; Copyright 2019 Hunter Damron

(load "solutions.lisp")

(defun single-test (f)
  (lambda (params correct)
    (let* ((ans (apply f params))
           (comment (if (equalp ans correct) "" (format nil " (Correct answer ~A)" correct))))
      (format t "  ~{~A~^ ~} -> ~A~A~%" params ans comment))))

(defun problem (num f tests)
  (format t "Problem ~2,'0D:~%" num)
  (mapcar (lambda (test) (apply (single-test f) test)) tests))

(defun problem1 (num f params correct)
  (format t "Problem ~2,'0D:~%" num)
  (funcall (single-test f) params correct))

(defun main ()
  (problem1 1 'my-last '((3 1 4 9)) 9)
  (problem1 2 'my-but-last '((3 1 4 9)) 4)
  (problem 3 'element-at '((((3 1 4 8 7 2 13) 3) 8)))
  (problem 4 'my-len '((((3 1 4 9)) 4)
                       ((())        0)))
  (problem 5 'my-rev '((((3 1 4 9)) (9 4 1 3))
                       ((())     ())))
  (problem 6 'palindrome '((((1 2 3 2 1)) t)
                           (((1 2 4 2 3)) nil)))
  (problem1 7 'my-flatten '((1 2 (3 2) () 1 8 (1 (2 3)))) '(1 2 3 2 1 8 1 2 3)))
