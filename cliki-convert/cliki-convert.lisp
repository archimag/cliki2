;;; WARNING!
;;; use "convmv -f latin1 -t utf8 --nosmart --replace --notest *"
;;; for prepare old cliki pages

(in-package #:cliki2.converter)

;;;; prepare old cliki article

(defun htmlize-old-cliki-page (file &aux (str (alexandria:read-file-into-string file :external-format :latin1)))
  (with-output-to-string (out)
    (dolist (p (ppcre:split "\\n\\s*\\n" str))
      (format out "<p>~A</p>" p))))

;;;; covert old cliki article

(defun convert-old-cliki-page (file)
  (remove-artefacts
   (with-input-from-string (in (htmlize-old-cliki-page file))
     (with-output-to-string (s)
       (external-program:run "/usr/bin/pandoc"
                             (list "-f" "html" "-t" "markdown" "-")
                             :input in
                             :output s)))))

;;;; load-old-articles

(defun load-old-articles (old-article-dir &key verbose)
  "WARNING: This WILL blow away your old store."
  (close-store)
  (cliki2::close-search-index)

  (iter (for item in '("content/" "index/" "store/"))
        (for path = (merge-pathnames item cliki2::*datadir*))
        (cl-fad:delete-directory-and-files path
                                           :if-does-not-exist :ignore)
        (ensure-directories-exist path))

  (open-store (merge-pathnames "store/" cliki2::*datadir*))
  (cliki2::open-search-index)

  (let ((old-articles (make-hash-table :test 'equalp))) ;; equalp is case insensitive
    (dolist (file (cl-fad:list-directory old-article-dir))
      (let ((file-name (hunchentoot:url-decode
                        (substitute #\% #\= (pathname-name file))
                        hunchentoot::+latin-1+)))
        (setf (gethash file-name old-articles)
              (merge 'list
                     (list file)
                     (gethash file-name old-articles)
                     #'< :key (lambda (x)
                                (or (ignore-errors (parse-integer (pathname-type x)))
                                    0))))))
    ;; sort revisions and discard deleted pages
    (loop for article being the hash-key of old-articles do
         (when (search "*(delete this page)"
                       (alexandria:read-file-into-string
                        (car (last (gethash article old-articles)))
                        :external-format :latin1)
                       :test #'char-equal)
           (remhash article old-articles)))
    ;; import into store
    (let ((cliki-import-user (make-instance 'cliki2::user
                                            :name "CLiki-importer"
                                            :email "noreply@cliki.net"
                                            :password-salt #(0 0 0 0)
                                            :password-digest "nohash")))
      (loop for i from 0
         for article-title being the hash-key of old-articles do
           (let ((article (make-instance 'cliki2::article :title article-title))
                 (content nil)
                 (timestamp-skew 0)) ;; needed because some revisions have identical timestamps
             (when verbose
               (format t
                       "~A%; Convert ~A~%"
                       (floor (* (/ i (hash-table-count old-articles)) 100))
                       article-title))
             (dolist (file (gethash article-title old-articles))
               (cliki2::add-revision article
                                     "import from CLiki"
                                     (setf content (convert-old-cliki-page file))
                                     :author cliki-import-user
                                     :author-ip "0.0.0.0"
                                     :date (+ (incf timestamp-skew)
                                              (file-write-date file))
                                     :add-to-index nil))
             (cliki2::add-article-to-index article-title content)))))
  ;; fix up recent revisions (this will get blown away on store reload, but doesn't matter because all revisions made after import will be correct)
  (cliki2::init-recent-revisions)
  (snapshot))

;; (load-old-articles "/home/viper/tmp/cliki/")
