;;; cliki-convert.lisp
;;;
;;; WARNING!
;;; use "convmv -f latin1 -t utf8 --nosmart --replace --notest *"
;;; for prepare old cliki pages

(in-package #:cliki2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; prepare old cliki article
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun htmlize-old-cliki-page (file &aux (str (alexandria:read-file-into-string file :external-format :latin1)))
  (with-output-to-string (out)
    (dolist (p (ppcre:split "\\n\\s*\\n" str))
      (format out "<p>~A</p>" p))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; covert old cliki article
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun convert-old-cliki-page (file)
  (cliki2.converter.artefacts:remove-artefacts
   (with-input-from-string (in (htmlize-old-cliki-page file))
     (with-output-to-string (s)
       (external-program:run "/usr/bin/pandoc"
                             (list "-f" "html" "-t" "markdown" "-")
                             :input in
                             :output s)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; load-old-articles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun load-old-articles (old-article-dir &key verbose)
  "WARNING: This WILL blow away your old store."
  (let ((old-articles (make-hash-table :test 'equal))
        (store-dir (merge-pathnames "store/" *datadir*)))
    (close-store)
    (ensure-directories-exist store-dir)
    (cl-fad:delete-directory-and-files store-dir)
    (open-store store-dir)
    (dolist (file (cl-fad:list-directory old-article-dir))
      (push file (gethash (string-downcase (hunchentoot:url-decode (substitute #\% #\= (pathname-name file))
                                                                   hunchentoot::+latin-1+))
                          old-articles)))
    ;; sort revisions and discard deleted pages
    (loop for article being the hash-key of old-articles do
         (setf (gethash article old-articles)
               (sort (gethash article old-articles) #'string< :key #'file-namestring))
         (when (search "*(delete this page)"
                       (alexandria:read-file-into-string
                        (car (last (gethash article old-articles)))
                        :external-format :latin1)
                       :test #'char-equal)
           (remhash article old-articles)))
    ;; import into store
    (let ((cliki-import-user (make-instance 'user
                                            :name "CLiki-import"
                                            :email "noreply@cliki.net"
                                            :password "nohash")))
      (loop for i from 0
         for article-title being the hash-key of old-articles do
           (let ((article (make-instance 'article :title article-title))
                 (content nil)
                 (timestamp-skew 0)) ;; needed because some revisions have identical timestamps
             (when verbose
               (format t
                       "~A%; Convert ~A~%"
                       (floor (* (/ i (hash-table-count old-articles)) 100))
                       article-title))
             (dolist (file (sort (copy-list (gethash article-title old-articles))
                                 #'<
                                 :key #'file-write-date))
               (add-revision article
                             "CLiki import"
                             (setf content (convert-old-cliki-page file))
                             :author cliki-import-user
                             :author-ip "0.0.0.0"
                             :date (+ (incf timestamp-skew) (file-write-date file))
                             :add-to-index nil))
             (add-article-to-index article-title content)))))
  ;; fix up recent revisions
  (replace *recent-revisions* (sort (store-objects-with-class 'revision) #'< :key #'revision-date))
  (setf *recent-revisions-latest* 99))

;; (load-old-articles "/home/viper/tmp/cliki/")

