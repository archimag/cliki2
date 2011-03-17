(in-package #:cliki2)

(defpackage #:cliki2.categories)

(defun category-keyword (category-title)
  (intern (string-upcase category-title) '#:cliki2.categories))

(defun content-categories (markup)
  (mapcar
   (lambda (x)
     (subseq x 2 (- (length x) 1)))
   (cl-ppcre:all-matches-as-strings
    '(:sequence #\* #\( (:greedy-repetition 0 nil (:inverted-char-class #\))) #\))
    markup)))
