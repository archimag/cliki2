;;;; utils.lisp

(in-package #:cliki2)

;;;; utime

(cffi:defcstruct %utimbuf
  (%actime isys:time-t)
  (%modtime isys:time-t))

(isys:defsyscall (%utime "utime") :int
  (filename :pointer)
  (times :pointer))

(defun utime (filename &optional access-time modification-time)
  (cffi:with-foreign-string (%filename (cffi-sys:native-namestring filename))
    (cond
      ((not (and access-time modification-time))
       (%utime %filename (cffi:null-pointer)))
      (t (cffi:with-foreign-object (utimbuf '%utimbuf)
           (setf (cffi:foreign-slot-value utimbuf '%utimbuf '%actime)
                 access-time)
           (setf (cffi:foreign-slot-value utimbuf '%utimbuf '%modtime)
                 modification-time)
           (%utime %filename utimbuf))))))

;;;; sendmail

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

;;;; format-time

(defun format-time (universal-time)
  (apply #'format nil
         "~D-~2,'0D-~2,'0D ~2,'0D:~2,'0D"
         (reverse (subseq (multiple-value-list (decode-universal-time universal-time)) 1 6))))