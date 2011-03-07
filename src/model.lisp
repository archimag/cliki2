;;;; model.lisp

(in-package #:cliki2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; cliki2 storage
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass cliki2-store (mp-store)
  ((recent-changes :initform nil :accessor cliki2-recent-changes)))

(defmethod bknr.datastore::restore-store :before ((store cliki2-store) &key until)
  (declare (ignore until))
  (setf (cliki2-recent-changes store) nil))

(deftransaction add-change (object user date comment)
  (push (list object user date comment)
        (cliki2-recent-changes *store*)))

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
   (password :initarg :password
             :accessor user-password))
  (:metaclass persistent-class))

(defun user-info-pathname (user)
  (merge-pathnames (format nil
                           "person/~A"
                           (hunchentoot:url-encode (user-name user)))
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
;;;; revision
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass revision (store-object)
  ((author :initarg :author
           :initform nil
           :reader revision-author)
   (author-ip :initarg :author-ip
              :initform nil
              :reader revision-author-ip)
   (date :initarg :date
         :initform (get-universal-time)
         :reader revision-date)
   (content-sha1 :initarg :content-sha1
                 :initform ""
                 :reader revision-content-sha1))
  (:metaclass persistent-class))

(defun content-path (sha1)
  (merge-pathnames (format nil "content/~A/~A" (subseq sha1 0 2) (subseq sha1 2))
                   *datadir*))

(defun save-content (content)
  (let* ((octets (babel:string-to-octets content :encoding :utf-8))
         (sha1 (ironclad:byte-array-to-hex-string
                (ironclad:digest-sequence :sha1 octets)))
         (path (content-path sha1)))
    (unless (fad:file-exists-p path)
      (ensure-directories-exist path)
      (alexandria:write-byte-vector-into-file octets path))
    sha1))

;; (defmethod shared-initialize :after ((revision revision) slot-names &key content &allow-other-keys)
;;   (when content
;;     (setf (slot-value revision 'content-sha1)
;;           (save-content content))))

(defun revision-content (revision)
  (alexandria:read-file-into-string
   (content-path (revision-content-sha1 revision))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; article
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass article (store-object)
  ((title :initarg :title
          :reader article-title)
   (downcase-title :index-type string-unique-index
                   :index-reader article-with-downcase-title
                   :index-values all-articles)
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

(defun article-last-revision (article)
  (car (article-revisions article)))
   
(defun article-content (article)
  (revision-content (article-last-revision article)))

