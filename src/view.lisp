;;;; render.lisp

(in-package #:cliki2)

(defgeneric render-article-revision (drawer article revision mode))

(defgeneric render-handle-markup (drawer content))

(defgeneric apply-template (drawer template &rest args &key &allow-other-keys))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; drawer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass drawer () ())

(defmethod apply-template ((drawer drawer) template &rest args &key &allow-other-keys)
  (funcall template
   (concatenate 'list
                args
                (list :user  
                      (if *user*
                          (list :name (user-name *user*)
                                :href (restas:genurl 'view-person
                                                     :name (user-name *user*))
                                :sign-out (restas:genurl 'sign-out))
                          (list :register (restas:genurl 'register)
                                :sign-in (restas:genurl 'sign-in)
                                :callback (hunchentoot:referer)))))))

(defmethod render-handle-markup ((drawer drawer) content)  
  (generate-html-from-markup content))

(defun article-action-list (article mode
                            &aux (title (article-title article)))
  (case mode
    (:view (list :edit (restas:genurl 'edit-article :title title)
                 :raw (restas:genurl 'view-article-source :title title)
                 :history (restas:genurl 'view-article-history :title title)))
    
    (:history (list :view (restas:genurl 'view-article :title title)))

    (:revision (list :view (restas:genurl 'view-article :title title)
                     :history (restas:genurl 'view-article-history :title title)))))


(defmethod render-article-revision ((drawer drawer) (article article) (revision revision) mode)
  (let ((title (article-title article)))
    (apply-template drawer
                    'cliki2.view:view-article
                    :title title
                    :content (render-handle-markup drawer (revision-content revision))
                    :links (article-action-list article mode))))

;; article

(defmethod restas:render-object ((drawer drawer) (article article))
  (render-article-revision drawer article (article-last-revision article) :view))

;; edit-article-page

(defclass edit-article-page ()
  ((title :initarg :title :reader article-title)
   (article :initarg :article :reader article)))

(defmethod restas:render-object ((drawer drawer) (page edit-article-page))
  (let* ((title (article-title page))
         (article (article page)))
    (apply-template drawer
                    'cliki2.view:edit-article
                    :title title
                    :content (if article
                                 (article-content article)
                                 ""))))

;; article-not-found

(defclass article-not-found ()
  ((title :initarg :title :reader article-title)))

(defmethod restas:render-object ((drawer drawer) (article article-not-found)
                                 &aux (title (article-title article)))
  (apply-template drawer
                  'cliki2.view:article-not-found
                  :title title
                  :create-link (restas:genurl 'edit-article :title title)))

;; article-preview-page

(defclass preview-article-page ()
  ((title :initarg :title :reader article-title)
   (content :initarg :content :reader preview-article-content)))

(defmethod restas:render-object ((drawer drawer) (page preview-article-page))
  (apply-template drawer
                  'cliki2.view:edit-article
                  :title (article-title page)
                  :content (preview-article-content page)
                  :preview (generate-html-from-markup (preview-article-content page))))

;; article-history-page

(defclass article-history-page ()
  ((article :initarg :article :reader article)))

(defmethod restas:render-object ((drawer drawer) (page article-history-page)
                                 &aux
                                 (article (article page))
                                 (title (article-title article)))
  (apply-template drawer
                  'cliki2.view:view-article-history
                  :title (format nil "History of page \"~A\"" title)
                  :history (iter (for revision in (article-revisions article))
                                 (collect
                                     (list :href (restas:genurl 'view-article-revision
                                                                :title title
                                                                :mark (revision-content-sha1 revision))
                                           :author (let ((name (user-name (revision-author revision))))
                                                     (list :name name
                                                           :href (restas:genurl 'view-person
                                                                                :name name)))
                                           :date (hunchentoot:rfc-1123-date (revision-date revision)))))
                  :links (article-action-list article :history)))

;; article-revision-page

(defclass article-revision-page ()
  ((article :initarg :article :reader article)
   (revision :initarg :revision :reader revision)))

(defmethod restas:render-object ((drawer drawer) (page article-revision-page))
  (render-article-revision drawer
                           (article page)
                           (revision page)
                           :revision))

;; login

(defmethod restas:render-object ((drawer drawer) (page (eql :sign-in-page)))
  (apply-template drawer
                  'cliki2.view:sign-in
                  :forgot-href (restas:genurl 'forgot)))

;; register

(defclass register-page ()
  ((data :initarg :data :initform nil :reader register-data)))

(defmethod restas:render-object ((drawer drawer) (page register-page))
  (apply-template drawer
                  'cliki2.view:register
                  :data (list* :recaptcha-pubkey *recaptcha.publick-key*
                               (register-data page))))

;; register-sendmail-page

(defmethod restas:render-object ((drawer drawer) (page (eql :register-sendmail-page)))
  (apply-template drawer  
                  'cliki2.view:register-continue))

;; confirm-registration

(defmethod restas:render-object ((drawer drawer) (page (eql :confirm-registration-page)))
  (apply-template drawer  
                  'cliki2.view:confirm-registration))
  
;; person

(defmethod restas:render-object ((drawer drawer) (person user)
                                 &aux (name (user-name person)))
  (apply-template drawer  
                  'cliki2.view:view-person
                  :title name
                  :content (render-handle-markup drawer (user-info person))
                  :edit-link (if (and *user*
                                      (string= name (user-name *user*)))
                                 (restas:genurl 'edit-person :name name))))

;; edit-person-page

(defclass edit-person-page ()
  ((person :initarg :person :reader person)))

(defmethod restas:render-object ((drawer drawer) (page edit-person-page)
                                 &aux (person (person page)))
  (apply-template drawer  
                  'cliki2.view:edit-person
                  :title (user-name person)
                  :content (user-info person)))

;; preview-person-page

(defclass preview-person-page ()
  ((person :initarg :person :reader person)
   (info :initarg :info :reader preview-person-info)))

(defmethod restas:render-object ((drawer drawer) (page preview-person-page))
  (apply-template drawer  
                  'cliki2.view:edit-person
                  :title (user-name (person page))
                  :content (preview-person-info page)
                  :preview (generate-html-from-markup (preview-person-info page))))

;; forbidden

(defmethod restas:render-object ((drawer drawer) (code (eql hunchentoot:+http-forbidden+)))
  (apply-template drawer
                  'cliki2.view:forbidden
                  :uri (hunchentoot:request-uri*)))