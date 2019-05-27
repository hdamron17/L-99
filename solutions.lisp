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
  (if (<= k 1)
    (car l)
    (element-at (cdr l) (1- k))))

(defun my-len (l)
  "P04 Find the number of elements of a list"
  (reduce (lambda (a b) (declare (ignore b)) (1+ a)) (cons 0 l)))

(defun my-rev (l)
  "P05 Reverse a list"
  (reduce (lambda (a b) (cons b a)) (cons '() l)))

(defun palindrome (l)
  "P06 Find out whether a list is a palindrome"
  (every 'eq l (my-rev l)))

(defun layered-cons (acc l)
  (if (and (atom l) (not (null l)))
    (cons l acc)
    (reduce 'layered-cons (cons acc l))))
(defun my-flatten (l)
  "P07 Flatten a nested list structure"
  (my-rev (layered-cons '() l)))

(defun compose (f &rest others)
  (reduce (lambda (f1 f2) (lambda (p) (funcall f1 (funcall f2 p)))) (cons f others)))
(defun compress (l)
  "P08 Eliminate consecutive duplicates of list elements"
  (if (null l) l
    (cons (car l) (mapcar (compose 'car 'cdr) (remove-if (lambda (a) (apply 'eq a)) (mapcar 'list l (cdr l)))))))

(defun ~pack (packed v)
  (let ((pack-car (car packed)))
    (if (eq v (car pack-car))
      (cons (cons v pack-car) (cdr packed))
      (cons (list v) packed))))
(defun pack (l)
  "P09 Pack consecutive duplicates of list elements into sublists"
  (my-rev (reduce '~pack (cons `((,(car l))) (cdr l)))))

(defun encode (l)
  "P10 Run-length encoding of a list"
  (mapcar (lambda (subl) (list (my-len subl) (car subl))) (pack l)))

(defun 2nd (pair) (car (cdr pair)))
(defun encode-singletons (encl)
  "Replaces (1 X) with X in encoded list"
  (mapcar (lambda (nv) (if (= (car nv) 1) (2nd nv) nv)) encl))
(defun encode-modified (l)
  "P11 Modified run-length encoding"
  (encode-singletons (encode l)))

(defun dupn (n v)
  "Creates list of n duplicates of v"
  (when (> n 0) (cons v (dupn (1- n) v))))
(defun decode-single (enc)
  "Converts a single encoded value (N X) or X to a list of duplicates"
  (if (atom enc)
    (list enc)
    (dupn (car enc) (2nd enc))))
(defun decode (l)
  "P12 Decode a run-length encoded list"
  (apply 'append (mapcar 'decode-single l)))

(defun ~encode-direct (encoded v)
  (let* ((encoded-car (car encoded))
         (prev-n (car encoded-car))
         (prev-v (2nd encoded-car)))
    (if (eq v prev-v)
      (cons (list (1+ prev-n) prev-v) (cdr encoded))
      (cons (list 1 v) encoded))))
(defun encode-direct (l)
  "P13 Run-length encoding of a list (direct solution)"
  (encode-singletons (my-rev (reduce '~encode-direct (cons `((1 ,(car l))) (cdr l))))))

; P14 is a special case of P15 so it is provided after
(defun repli (l n)
  "P15 Replicate the elements of a list a given number of times"
  (apply 'append (mapcar (lambda (v) (dupn n v)) l)))

(defun dupli (l)
  "P14 Duplicate the elements of a list"
  (repli l 2))

(defun ~range1 (i n) (when (< i n) (cons i (~range1 (1+ i) n))))
(defun range1 (n) (~range1 0 n))
(defun enumerate (l) (mapcar 'list (range1 (my-len l)) l))
(defun drop (l n)
  "P16 Drop every N'th element from a list"
  (mapcar '2nd (remove-if (lambda (v) (= (rem (1+ (car v)) n) 0)) (enumerate l))))

(defun ~split (left right n)
  "Shift values from right to left n times"
  (if (> n 0)
    (let ((v (car right))) (~split (cons v left) (cdr right) (1- n)))
    (list left right)))
(defun split (l n)
  "P17 Split a list into two parts; the length of the first part is given"
  (let ((funnel (~split '() l n))) (list (my-rev (car funnel)) (2nd funnel))))

(defun slice (l start end)
  "P18 Extract a slice from a list"
  (mapcar '2nd (remove-if (lambda (nv) (let ((n (1+ (car nv)))) (or (< n start) (> n end)))) (enumerate l))))

(defun rotate (l n)
  "P19 Rotate a list N places to the left"
  (apply 'append (my-rev (split l (mod n (my-len l))))))

(defun remove-at (l k)
  "P20 Remove the K'th element from a list"
  (mapcar '2nd (remove-if (lambda (v) (= (1+ (car v)) k)) (enumerate l))))

(defun insert-at (val l k)
  "P21 Insert an element at a given position into a list"
  (let ((sl (split l (1- k))))
    (apply 'append (list (car sl) (list val) (2nd sl)))))

(defun range (start end)
  "P22 Create a list containing all integers within a given range"
  (mapcar (lambda (n) (+ n start)) (range1 (1+ (- end start)))))

(defun rnd-select (l n)
  "P23 Extract a given number of randomly selected elements from a list"
  (let ((i (random (my-len l))))
    (when (> n 0) (cons (element-at l i) (rnd-select (remove-at l i) (1- n))))))

(defun lotto-select (n m)
  "P24 Lotto: Draw N different random numbers from the set 1..M"
  (rnd-select (range 1 m) n))

(defun rnd-permu (l)
  "P25 Generate a random permutation of the elements of a list"
  (rnd-select l (my-len l)))

(defun select-one (l)
  "For list (a b c...) constructs pairs (a (b c...)), (b (a c...)), and so on"
  (mapcar (lambda (nv) (list (2nd nv) (remove-at l (1+ (car nv))))) (enumerate l)))
(defun ~combination (i prev n l)
    (when (not (null l))
      (if (= i n) (list prev)
        (when (< i n) (apply 'append (mapcar
          (lambda (selected-rest) (~combination (1+ i) (cons (car selected-rest) prev) n (2nd selected-rest)))
          (select-one l)))))))
(defun combination (n l)
  "P26 Generate the combinations of K distinct objects chosen from the N elements of a list"
  (mapcar 'my-rev (~combination 0 '() n l)))
