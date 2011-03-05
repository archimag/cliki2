;;;; model.lisp

(in-package #:cliki2)

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
             :accessor user-password)
   (info :initarg :info
         :initarg nil
         :accessor user-info))
  (:metaclass persistent-class))

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
   (content-sha1 :initarg :content
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

(defmethod shared-initialize :after ((revision revision) slot-names &key content &allow-other-keys)
  (when content
    (setf (slot-value revision 'content-sha1)
          (save-content content))))

(defun revision-content (revision)
  (alexandria:read-file-into-string
   (content-path (revision-content-sha1 revision))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; article
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass article (store-object)
  ((title :initarg :title
          :reader article-title
          :index-type string-unique-index
          :index-reader article-with-title
          :index-values all-articles)
   (revisions :initarg :revisions
              :initform nil
              :accessor article-revisions))
  (:metaclass persistent-class))

(defun article-last-revision (article)
  (car (article-revisions article)))
   
(defun article-content (article)
  (revision-content (article-last-revision article)))