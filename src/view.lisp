;;;; render.lisp

(in-package #:cliki2)

(defgeneric render-article-revision (drawer article revision mode))

(defgeneric render-handle-markup (drawer content))

(defgeneric apply-template (drawer template &rest args &key &allow-other-keys))

(defgeneric render-key-data (drawer pagetype &rest args &key &allow-other-keys))

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

;; key data

(defmethod restas:render-object ((drawer drawer) (data list))
  (apply 'render-key-data
         drawer
         (car data)
         (cdr data)))

;; article

(defmethod restas:render-object ((drawer drawer) (article article))
  (render-article-revision drawer article (article-last-revision article) :view))

;; edit-article-page

(defmethod render-key-data ((drawer drawer) (type (eql :edit-article-page))
                            &key title article)
  (apply-template drawer
                  'cliki2.view:edit-article
                  :title title
                  :content (if article
                               (article-content article)
                               "")))

;; article-not-found

(defmethod render-key-data ((drawer drawer) (type (eql :article-not-found-page)) &key title)
  (apply-template drawer
                  'cliki2.view:article-not-found
                  :title title
                  :create-link (restas:genurl 'edit-article :title title)))

;; article-preview-page

(defmethod render-key-data ((drawer drawer) (type (eql :preview-article-page))
                            &key title content)
  (apply-template drawer
                  'cliki2.view:edit-article
                  :title title
                  :content content
                  :preview (generate-html-from-markup content)))

;; article-history-page

(defmethod render-key-data ((drawer drawer) (type (eql :article-history-page))
                            &key article &aux (title (article-title article)))
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

(defmethod render-key-data ((drawer drawer) (type (eql :article-revision-page))
                            &key article revision)
  (render-article-revision drawer article revision :revision))

;; login

(defmethod restas:render-object ((drawer drawer) (page (eql :sign-in-page)))
  (apply-template drawer
                  'cliki2.view:sign-in
                  :forgot-href (restas:genurl 'forgot)))

;; register

(defmethod render-key-data ((drawer drawer) (type (eql :register-page))
                            &key data)
  (apply-template drawer
                  'cliki2.view:register
                  :data (list* :recaptcha-pubkey *recaptcha.publick-key*
                               data)))

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

(defmethod render-key-data ((drawer drawer) (type (eql :edit-person-page))
                            &key person)
  (apply-template drawer  
                  'cliki2.view:edit-person
                  :title (user-name person)
                  :content (user-info person)))

;; preview-person-page

(defmethod render-key-data ((drawer drawer) (type (eql :preview-person-page))
                            &key person info)
  (apply-template drawer  
                  'cliki2.view:edit-person
                  :title (user-name person)
                  :content info
                  :preview (generate-html-from-markup info)))

;; forbidden

(defmethod restas:render-object ((drawer drawer) (code (eql hunchentoot:+http-forbidden+)))
  (apply-template drawer
                  'cliki2.view:forbidden
                  :uri (hunchentoot:request-uri*)))

;; internal server error

(defmethod restas:render-object ((drawer drawer) (code (eql hunchentoot:+http-internal-server-error+)))
  (apply-template drawer
                  'cliki2.view:internal-server-error))
