(in-package #:cliki2)

(open-store (merge-pathnames "store/" *datadir*))

(defvar *cliki-import-user*)

(defun setup-import-user ()
  (setf *cliki-import-user*
        (make-instance 'user :name "CLiki-import" :email "noreply@cliki.net" :password "nohash")))

(defparameter *old-articles* (make-hash-table :test 'equal))

(defun load-old-articles (old-article-dir)
  ;; load up pathnames
  (dolist (file (cl-fad:list-directory old-article-dir))
    (push file (gethash (closure-template:decode-uri (pathname-name file)) *old-articles*)))
  ;; sort revisions and discard deleted pages
  (loop for article being the hash-key of *old-articles* do
       (setf (gethash article *old-articles*) (sort (gethash article *old-articles*) #'string< :key #'file-namestring))
       (when (search "*(delete this page)"
                     (alexandria:read-file-into-string (car (last (gethash article *old-articles*)))) :test #'char-equal)
         (remhash article *old-articles*)))
  ;; import into store
  (loop for article-title being the hash-key of *old-articles* do
     (let ((article (make-instance 'article :title article-title))
           (timestamp-skew 0)) ;; needed because some revisions have identical timestamps
       (dolist (file (gethash article-title *old-articles*))
         (add-revision article
                       "CLiki import"
                       (with-output-to-string (s)
                         (external-program:run "/usr/bin/pandoc"
                                               (list "-f" "html" "-t" "rst" file)
                                               :output s))
                       :author *cliki-import-user*
                       :author-ip "0.0.0.0"
                       :date (+ (incf timestamp-skew) (file-write-date file))))))
  ;; fix up recent revisions
  (replace *recent-revisions* (sort (store-objects-with-class 'revision) #'< :key #'revision-date))
  (setf *recent-revisions-latest* 99))

;; (load-old-articles "/home/viper/tmp/cliki/")

