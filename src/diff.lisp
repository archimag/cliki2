;;;; diff.lisp

(in-package #:cliki2)

(defclass wiki-diff (diff::diff) ())

(defclass wiki-diff-window (diff::diff-window) ())

(defmethod diff::create-window-for-diff ((wiki wiki-diff))
  (make-instance 'wiki-diff-window))

(defmethod diff::print-diff-window-header ((window wiki-diff-window) stream)
  (write-line (cliki2.view:diff-line-number
               (list :origin-start (diff::original-start-line window)
                     :modified-start (diff::modified-start-line window)))
              stream))

(defmethod diff::print-diff-header ((diff wiki-diff) stream)
  )

(defmethod print-object :after ((diff wiki-diff) stream)
  )

(defmethod print-object ((window wiki-diff-window) stream)
  (iter (for origin in (collect-origin (diff::window-chunks window)))
        (for modified in (collect-modified (diff::window-chunks window)))
        (cond
          ((string= origin modified)
           (write-string (cliki2.view:diff-common-line
                          (list :line origin))
                         stream))
          
          ((and origin modified)
           (let ((diff (com.gigamonkeys.prose-diff::diff-vectors
                        (closure-template:escape-html origin)
                        (closure-template:escape-html modified))))
             (write-line (cliki2.view:diff-line
                          (list :origin (format-diff-part diff :delete) 
                                :modified (format-diff-part diff :add)))
                         stream)))
                          
          (t (write-string (cliki2.view:diff-line
                            (list :origin (closure-template:escape-html origin)
                                  :modified (closure-template:escape-html modified)))
                           stream)))))

(defun collect-origin (chunks)
  (iter (for chunk in chunks)
        (case (diff::chunk-kind chunk)
          ((:common :delete)
           (dolist (line (diff::chunk-lines chunk))
             (collect line)))
          (:replace
           (collect (format nil "~{~&~A~}" (diff::chunk-lines chunk))))
          (:create
           (dolist (line (diff::chunk-lines chunk))
             (declare (ignore line))
             (collect nil))))))

(defun collect-modified (chunks)
  (iter (for chunk in chunks)
        (case (diff::chunk-kind chunk)
          ((:common  :create)
           (dolist (line (diff::chunk-lines chunk))
             (collect line)))
          (:insert
           (collect (format nil "~{~&~A~}" (diff::chunk-lines chunk))))
          (:delete 
           (dolist (line (diff::chunk-lines chunk))
             (declare (ignore line))
             (collect nil))))))
 
(defun concat-chunks (window exclude)
  (with-output-to-string (out)
    (dolist (chunk (diff::window-chunks window))
     (unless (find (diff::chunk-kind chunk) exclude)
       (dolist (line (diff::chunk-lines chunk))
         (write-string line out)
         (terpri out))))))

(defun format-diff-part (vector flag)
  (with-output-to-string (out)
    (iter (for item in (coerce vector 'list))
          (with tag = nil)

          (when (and tag
                     (not (eql (car item) flag)))
            (setf tag nil)
            (write-string "</span>"  out))

          (when (and (not tag)
                     (eql (car item) flag))
            (setf tag t)
            (write-string "<span>" out))

          (when (or (eql (car item) :lcs)
                    (eql (car item) flag))
            (if (eql (cdr item) #\Newline)
                (write-string "<br />" out)
                (write-char (cdr item) out))))))

(defun diff (old new)
  (let ((diff:*diff-context-lines* 2))
    (write-to-string
     (diff:generate-diff 'cliki2::wiki-diff old new))))



