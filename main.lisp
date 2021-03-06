;;;; Copyright 2019 Hunter Damron

(load "solutions.lisp")

(defun single-test (f)
  (lambda (params correct)
    (let* ((ans (apply f params))
           (comment (if (equalp ans correct) "" (format nil " [Correct answer ~A]" correct))))
      (format t "  ~{~A~^ ~} -> ~A~A~%" params ans comment))))

(defun problem (num f tests)
  (format t "Problem ~2,'0D: ~A~%" num f)
  (mapcar (lambda (test) (apply (single-test f) test)) tests))

(defun problem1 (num f params correct)
  (format t "Problem ~2,'0D: ~A~%" num f)
  (funcall (single-test f) params correct))

(defun main ()
  (problem1 1 'my-last '((a b c d)) 'd)
  (problem1 2 'my-but-last '((a b c d)) 'c)
  (problem1 3 'element-at '((a b c d e) 3) 'c)
  (problem 4 'my-len '((((a b c d)) 4)
                       ((())        0)))
  (problem 5 'my-rev '((((a b c d)) (d c b a))
                       ((())        ())))
  (problem 6 'palindrome '((((x a m a x)) t)
                           (((x a a x)) t)
                           (((x a x y)) nil)))
  (problem1 7 'my-flatten '((a (b (c d) e))) '(a b c d e))
  (problem1 8 'compress '((a a a a b c c a a d e e e e)) '(a b c a d e))
  (problem1 9 'pack '((a a a a b c c a a d e e e e)) '((a a a a) (b) (c c) (a a) (d) (e e e e)))
  (problem1 10 'encode '((a a a a b c c a a d e e e e)) '((4 a) (1 b) (2 c) (2 a) (1 d) (4 e)))
  (problem1 11 'encode-modified '((a a a a b c c a a d e e e e)) '((4 a) b (2 c) (2 a) d (4 e)))
  (problem1 12 'decode '(((4 a) b (2 c) (2 a) d (4 e))) '(a a a a b c c a a d e e e e))
  (problem1 13 'encode-direct '((a a a a b c c a a d e e e e)) '((4 a) b (2 c) (2 a) d (4 e)))
  (problem1 14 'dupli '((a b c c d)) '(a a b b c c c c d d))
  (problem1 15 'repli '((a b c) 3) '(a a a b b b c c c))
  (problem1 16 'drop '((a b c d e f g h i k) 3) '(a b d e g h k))
  (problem1 17 'split '((a b c d e f g h i k) 3) '((a b c) (d e f g h i k)))
  (problem1 18 'slice '((a b c d e f g h i k) 3 7) '(c d e f g))
  (problem 19 'rotate '((((a b c d e f g h) 3)  (d e f g h a b c))
                        (((a b c d e f g h) -2) (g h a b c d e f))))
  (problem1 20 'remove-at '((a b c d) 2) '(a c d))
  (problem1 21 'insert-at '(alfa (a b c d) 2) '(a alfa b c d))
  (problem1 22 'range '(4 9) '(4 5 6 7 8 9))
  (problem1 23 'rnd-select '((a b c d e f g h) 3) '(e a c))  ; Based on random
  (problem1 24 'lotto-select '(6 49) '(35 38 4 27 33 17))    ; Based on random
  (problem1 25 'rnd-permu '((a b c d e f)) '(d a c b e f))   ; Based on random
  (problem1 26 'combination '(3 (a b c d e f)) "is long")
  (let* ((l       '(a b c d e f))
         (correct 120)
         (expr    `(length (combination 3 ',l)))
         (ans     (eval expr))
         (comment (if (= ans correct) "" (format nil " [Correct answer ~A]" correct))))
    (format t "* C(~D,3) = ~A = ~A~A~%" (length l) expr ans comment))
  )
