;;;; model.lisp

(in-package #:cliki2)

;;;; search index

(defparameter *search-index* nil)

(defun close-search-index ()
  (when *search-index*
    (montezuma:close *search-index*)
    (setf *search-index* nil)))


(defun open-search-index (&aux (dir (merge-pathnames "index/" *datadir*)))
  (close-search-index)
  (ensure-directories-exist dir)
  (setf *search-index*
        (make-instance 'montezuma:index
                       :path dir)))

(open-search-index)

(defun add-article-to-index (title content)
  (let ((doc (montezuma:get-document *search-index*
                                     (montezuma:make-term "title" title))))
    (cond
      (doc (setf (montezuma:document-value doc "content")
                 content))
      (t (montezuma:add-document-to-index *search-index*
                                          `(("title" . ,title)
                                            ("content" . ,content)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; user
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass user (store-object)
  ((name :initarg :name
         :index-type string-unique-index
         :index-reader user-with-name
         :index-values all-users
         :reader user-name)
   (email :initarg :email
          :index-type string-unique-index
          :index-reader user-with-email
          :reader user-email)
   (role :initarg :role
         :initform nil
         :accessor user-role)
   (salt :initform nil
         :accessor user-password-salt) ;; FIXME
   (password :initarg :password
             :accessor user-password)) ;; hash this PROPERLY
  (:metaclass persistent-class))

(defun user-info-pathname (user)
  (merge-pathnames (make-pathname :directory '(:relative "person")
                                  :name user)
                   *datadir*))

(defun user-info (user &aux (path (user-info-pathname user)))
  (if (fad:file-exists-p path)
      (alexandria:read-file-into-string path)
      ""))

(defun (setf user-info) (newvalue user)
  (alexandria:write-string-into-file newvalue
                                     (ensure-directories-exist
                                      (user-info-pathname user))
                                     :if-exists :supersede
                                     :if-does-not-exist :create))

(defclass invite (store-object)
  ((user :initarg :user
         :reader invite-user)
   (date :initarg :date
         :initform (get-universal-time)
         :reader invite-date)
   (mark :reader invite-mark
         :index-type string-unique-index
         :index-reader invite-with-mark
         :index-values all-invites))
  (:metaclass persistent-class))

(defmethod shared-initialize :after ((invite invite) slot-names &key
                                     &aux (user (invite-user invite)))
  (setf (slot-value invite 'mark)
        (ironclad:byte-array-to-hex-string
         (ironclad:digest-sequence :sha1
                                   (babel:string-to-octets
                                    (format nil
                                            "~A~A~A"
                                            (user-name user)
                                            (user-email user)
                                            (user-password user)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; article
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass article (store-object)
  ((title :initarg :title
          :reader article-title)
   (downcase-title :index-type string-unique-index
                   :index-reader article-with-downcase-title
                   :index-values all-articles
                   :reader article-downcase-title)
   (revisions :initarg :revisions
              :initform nil
              :accessor article-revisions)
   (category-list :initarg :category-list
                  :initform nil
                  :accessor article-category-list
                  :index-type hash-list-index
                  :index-reader articles-with-category))
  (:metaclass persistent-class))

(defmethod shared-initialize :after ((article article) slot-names &key &allow-other-keys)
  (setf (slot-value article 'downcase-title)
        (string-downcase (article-title article))))

(defun article-with-title (title)
  (article-with-downcase-title (string-downcase title)))

(defun article-latest-revision (article)
  (car (article-revisions article)))

(defvar *latest-revision-cache* (make-hash-table))
(defvar *latest-revision-cache-lock* (bt:make-lock))

(defun article-content (article)
  (let ((latest-revision (article-latest-revision article))
        (cache (bt:with-lock-held (*latest-revision-cache-lock*)
                 (gethash article *latest-revision-cache*))))
    (if (eq (car cache) latest-revision)
        (cdr cache)
        (let ((content (revision-content (article-latest-revision article))))
          (bt:with-lock-held (*latest-revision-cache-lock*)
            (setf (gethash article *latest-revision-cache*)
                  (cons latest-revision content)))
          content))))

(defun article-content-head (article &aux (count 200))
  (let* ((revision (article-latest-revision article))
         (path (content-path article (revision-date revision))))
    (with-output-to-string (out)
      (iter (for char in-file path using #'read-char)
            (for i from 0 below count)
            (write-char char out)))))
            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; revision
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass revision (store-object)
  ((article      :initarg   :article
                 :reader    revision-article)
   (author       :initarg   :author
                 :reader    revision-author)
   (author-ip    :initarg   :author-ip
                 :reader    revision-author-ip)
   (date         :initarg   :date
                 :reader    revision-date)
   (summary      :initarg   :summary
                 :reader    revision-summary))
  (:metaclass persistent-class))

(defun content-path (article date)
  (merge-pathnames (make-pathname :directory (list :relative
                                                   "content"
                                                   (closure-template:encode-uri-component (article-title article)))
                                  :name (write-to-string date))
                   *datadir*))

(defun revision-content (revision)
  (alexandria:read-file-into-string
   (content-path (revision-article revision)
                 (revision-date revision))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; changes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *recent-revisions* (make-array 100 :initial-element nil))
(defvar *recent-revisions-latest* 0)
(defvar *recent-revisions-lock* (bt:make-lock))

(defun init-recent-revisions ()
  (replace *recent-revisions* (sort (store-objects-with-class 'revision) #'> :key #'revision-date))
  (setf *recent-revisions-latest* 0))

(defun get-recent-revisions ()
  (bt:with-lock-held (*recent-revisions-lock*)
    (let ((l (length *recent-revisions*)))
      (loop for i from 0 below l appending
           (let ((revision (aref *recent-revisions* (mod (+ i *recent-revisions-latest*) l))))
             (when revision (list revision)))))))

(defun add-revision (article summary content &key
                     (author *user*)
                     (author-ip (hunchentoot:real-remote-addr))
                     (date (get-universal-time))
                     (add-to-index t))
  
  (alexandria:write-string-into-file content
                                     (ensure-directories-exist (content-path article date))
                                     :if-exists :supersede
                                     :if-does-not-exist :create)
                                     
  (add-revision-txn article
                    (make-instance 'revision
                                   :article article
                                   :author author
                                   :author-ip author-ip
                                   :date date
                                   :summary summary)
                    (content-categories content))
  
  (when add-to-index
    (add-article-to-index (article-title article)
                          content)))

(deftransaction add-revision-txn (article revision content-categories)
  (push revision (article-revisions article))
  ;; interning symbols has to be done in transaction because bknr.datastore hates symbols
  (setf (article-category-list article) (mapcar #'category-keyword content-categories))
  (bt:with-lock-held (*recent-revisions-lock*)
    (setf *recent-revisions-latest* (mod (1+ *recent-revisions-latest*) (length *recent-revisions*))
          (aref *recent-revisions* *recent-revisions-latest*) revision)))
