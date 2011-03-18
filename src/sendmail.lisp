;;;; sendmail.lisp

(in-package #:cliki2)

(defun prepare-subject (subject &optional (external-format :utf-8))
  (format nil
          "=?~A?B?~A?="
          external-format
          (base64:string-to-base64-string
           (coerce (loop for code across (babel:string-to-octets subject
                                                                 :encoding external-format)
                      collect (code-char code))
                   'string))))

(defun sendmail (to content)
  (with-input-from-string (i content)
    (external-program:run *sendmail* to :input i)))

