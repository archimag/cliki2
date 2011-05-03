;;;; diff.lisp

(in-package #:cliki2)

(defun format-revisions-diff (origin modified)
  (diff:format-diff-string 'wiki-diff origin modified))

(defclass wiki-diff (diff:diff) ()
  (:default-initargs
   :window-class 'wiki-diff-window))

(defclass wiki-diff-window (diff:diff-window) ())

(defmethod diff:render-diff-window :before ((window wiki-diff-window) stream)
  (write-line (cliki2.view:diff-line-number
               (list :origin-start (diff:original-start-line window)
                     :modified-start (diff:modified-start-line window)))
              stream))

(defmethod diff:render-diff-window ((window wiki-diff-window) stream)
  (iter (for origin in (select-origin-chunks (diff:window-chunks window)))
        (for modified in (select-modified-chunks (diff:window-chunks window)))
        (cond
          ((string= origin modified)
           (write-string (cliki2.view:diff-common-line
                          (list :line origin))
                         stream))
          
          ((and origin modified)
           (write-line (cliki2.view:diff-line
                        (simple-compare-strings (closure-template:escape-html origin)
                                                (closure-template:escape-html modified)))
                       stream))
                          
          (t (write-string (cliki2.view:diff-line
                            (list :origin (closure-template:escape-html origin)
                                  :modified (closure-template:escape-html modified)))
                           stream)))))

(defun select-origin-chunks (chunks)
  (iter (for chunk in chunks)
        (case (diff:chunk-kind chunk)
          ((:common :delete)
           (dolist (line (diff:chunk-lines chunk))
             (collect line)))
          (:replace
           (collect (format nil "~{~&~A~}" (diff:chunk-lines chunk))))
          (:create
           (dolist (line (diff:chunk-lines chunk))
             (declare (ignore line))
             (collect nil))))))

(defun select-modified-chunks (chunks)
  (iter (for chunk in chunks)
        (case (diff:chunk-kind chunk)
          ((:common  :create)
           (dolist (line (diff:chunk-lines chunk))
             (collect line)))
          (:insert
           (collect (format nil "~{~&~A~}" (diff:chunk-lines chunk))))
          (:delete 
           (dolist (line (diff:chunk-lines chunk))
             (declare (ignore line))
             (collect nil))))))
 
(defun simple-compare-strings (origin modified)
  (labels ((str2arr (str)
             (map 'simple-vector #'char-code str))
           (wrt (str out &key start end)
             (iter (for i from start below end)
                   (for ch = (char str i))
                   (if (char= ch #\Newline)
                       (write-line "<br />" out)
                       (write-char ch out))))
           (fmt (regions str offset-fun length-fun)
             (with-output-to-string (out)
               (iter (for reg in regions)
                     (for modified-p = (typep reg 'diff:modified-diff-region))
                     (for start = (funcall offset-fun reg))
                     (for end = (+ start (funcall length-fun reg)))

                     (when modified-p
                       (write-string "<span>" out))
                     
                     (wrt str out :start start :end end)
                     
                     (when modified-p
                       (write-string "</span>" out))))))
    (let ((rawdiff (diff:compute-raw-diff (str2arr origin)
                                          (str2arr modified))))
      (list :origin (fmt rawdiff origin #'diff:original-start #'diff:original-length)
            :modified (fmt rawdiff modified #'diff:modified-start #'diff:modified-length)))))
