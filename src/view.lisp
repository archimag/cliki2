;;;; render.lisp

(in-package #:cliki2)

(defgeneric render-article-revision (drawer article revision mode))

(defgeneric render-handle-markup (drawer content))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; links
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun user-info-links ()
  (list :user  
        (if *user*
            (list :name (user-name *user*)
                  :href (restas:genurl 'view-person
                                       :name (user-name *user*))
                  :sign-out (restas:genurl 'sign-out))
            (list :register (restas:genurl 'register)
                  :sign-in (restas:genurl 'sign-in)
                  :callback (hunchentoot:referer)))))

(defun article-action-list (article mode
                            &aux (title (article-title article)))
  (case mode
    (:view (list :edit (restas:genurl 'edit-article :title title)
                 :raw (restas:genurl 'view-article-source :title title)
                 :history (restas:genurl 'view-article-history :title title)))
    
    (:history (list :view (restas:genurl 'view-article :title title)))

    (:revision (list :view (restas:genurl 'view-article :title title)
                     :history (restas:genurl 'view-article-history :title title)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; drawer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass drawer () ())

(defmethod render-handle-markup ((drawer drawer) content)  
  (generate-html-from-markup content))

(defmethod render-article-revision ((drawer drawer) (article article) (revision revision) mode)
  (let ((title (article-title article)))
    (cliki2.view:view-article
     (list* :title title
            :content (render-handle-markup drawer (revision-content revision))
            :links (article-action-list article mode)
            (user-info-links)))))

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
    (cliki2.view:edit-article
     (list* :title title
            :content (if article
                         (article-content article)
                         "")
            (user-info-links)))))

;; article-not-found

(defclass article-not-found ()
  ((title :initarg :title :reader article-title)))

(defmethod restas:render-object ((drawer drawer) (article article-not-found)
                                 &aux (title (article-title article)))
  (cliki2.view:article-not-found
   (list* :title title
          :create-link (restas:genurl 'edit-article :title title)
          (user-info-links))))

;; article-preview-page

(defclass preview-article-page ()
  ((title :initarg :title :reader article-title)
   (content :initarg :content :reader preview-article-content)))

(defmethod restas:render-object ((drawer drawer) (page preview-article-page))
  (cliki2.view:edit-article
   (list* :title (article-title page)
          :content (preview-article-content page)
          :preview (generate-html-from-markup (preview-article-content page))
          (user-info-links))))

;; article-history-page

(defclass article-history-page ()
  ((article :initarg :article :reader article)))

(defmethod restas:render-object ((drawer drawer) (page article-history-page)
                                 &aux
                                 (article (article page))
                                 (title (article-title article)))
  (cliki2.view:view-article-history
   (list* :title (format nil "History of page \"~A\"" title)
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
          :links (article-action-list article :history)
          (user-info-links))))

;; login

(defmethod restas:render-object ((drawer drawer) (page (eql :sign-in-page)))
  (cliki2.view:sign-in
   (list* :forgot-href (restas:genurl 'forgot)
          (user-info-links))))

;; register

(defclass register-page ()
  ((data :initarg :data :initform nil :reader register-data)))

(defmethod restas:render-object ((drawer drawer) (page register-page))
  (cliki2.view:register
   (concatenate 'list
                (list :recaptcha-pubkey *recaptcha.publick-key*)
                (register-data page)
                (user-info-links))))
  
;; register-sendmail-page

(defmethod restas:render-object ((drawer drawer) (page (eql :register-sendmail-page)))
  (cliki2.view:register-continue
   (user-info-links)))

;; confirm-registration

(defmethod restas:render-object ((drawer drawer) (page (eql :confirm-registration-page)))
  (cliki2.view:confirm-registration
   (user-info-links)))
  
;; person

(defmethod restas:render-object ((drawer drawer) (person user)
                                 &aux (name (user-name person)))
  (cliki2.view:view-person
     (list* :title name
            :content (render-handle-markup drawer (user-info person))
            :edit-link (if (and *user*
                                (string= name (user-name *user*)))
                           (restas:genurl 'edit-person :name name))
            (user-info-links))))

;; edit-person-page

(defclass edit-person-page ()
  ((person :initarg :person :reader person)))

(defmethod restas:render-object ((drawer drawer) (page edit-person-page)
                                 &aux (person (person page)))
  (cliki2.view:edit-person
   (list* :title (user-name person)
          :content (user-info person)
          (user-info-links))))

;; preview-person-page

(defclass preview-person-page ()
  ((person :initarg :person :reader person)
   (info :initarg :info :reader preview-person-info)))

(defmethod restas:render-object ((drawer drawer) (page preview-person-page))
  (cliki2.view:edit-person
      (list* :title (user-name (person page))
             :content (preview-person-info page)
             :preview (generate-html-from-markup (preview-person-info page))
             (user-info-links))))
