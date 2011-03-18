;;;; artefacts.lisp

(defpackage #:cliki2.converter.artefacts
  (:use #:cl #:esrap)
  (:export #:remove-artefacts))

(in-package #:cliki2.converter.artefacts)

(defrule article-link (and #\\ "_(" (+ (and (! #\)) character)) #\))
  (:lambda (list)
    (concat (cdr list))))

(defrule category-link (and #\\ "*(" (+ (and (! #\)) character)) #\))
  (:lambda (list)
    (concat (cdr list))))

(defrule hyperspec-link (and "\\#" "H(" (+ (and (! #\)) character)) #\))
  (:lambda (list)
    (concat #\_ (cdr list))))

(defun category-char-p (character)
  (not (member character '(#\: #\" #\)))))

(defrule category-name (and (? #\") (+ (category-char-p character)) (? #\"))
  (:lambda (list)
    (concat (second list))))

(defrule category-list (and (and "/(")
                            category-name
                            (* (and (! #\)) character))                                
                            ")")
  (:lambda (list)
    (format nil "_/(~A)" (second list))))

(defrule remove-artefacts
    (* (or article-link
           category-link
           hyperspec-link
           category-list
           character))
  (:concat t))

(defun remove-artefacts (str)
  (parse 'remove-artefacts str))
