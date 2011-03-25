;;;; render.lisp

(in-package #:cliki2)

(defgeneric render-key-data (drawer pagetype &rest args &key &allow-other-keys))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; drawer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass drawer () ())

(defun apply-template (drawer template &rest args &key &allow-other-keys)
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

(defun article-action-list (article mode)
  (let ((title (article-title article)))
    (case mode
      (:view (list :edit (restas:genurl 'edit-article :title title)
                   :raw (restas:genurl 'view-article-source :title title)
                   :history (restas:genurl 'view-article-history :title title)))
      (:history (list :view (restas:genurl 'view-article :title title)))

      (:revision (list :view (restas:genurl 'view-article :title title)
                       :history (restas:genurl 'view-article-history :title title))))))


(defun render-article-revision (drawer article revision mode)
  (apply-template drawer
                  'cliki2.view:view-article
                  :title (article-title article)
                  :content (generate-html-from-markup (revision-content revision))
                  :links (article-action-list article mode)))


(defun revision-summary-list (revisions)
  (flet ((format-time (universal-time)
           (apply #'format nil
                  "~D-~2,'0D-~2,'0D ~2,'0D:~2,'0D"
                  (reverse (subseq (multiple-value-list (decode-universal-time universal-time)) 1 6)))))
    (loop for revision in revisions collect
         (let ((title (article-title (revision-article revision))))
           (list :href (restas:gen-full-url 'view-article-revision
                                            :title title
                                            :mark (revision-content-sha1 revision))
                 :date (format-time (revision-date revision))
                 :author (let ((name (user-name (revision-author revision))))
                           (list :name name
                                 :href (restas:genurl 'view-person
                                                      :name name)))
                 :title title
                 :summary (revision-summary revision))))))

;; article


(defmethod restas:render-object ((drawer drawer) (article article))
  (apply-template drawer
                  'cliki2.view:view-article
                  :title (article-title article)
                  :content (generate-html-from-markup (article-content article))
                  :links (article-action-list article :view)))

;; key data

(defmethod restas:render-object ((drawer drawer) (data list))
  (apply 'render-key-data drawer (car data) (cdr data)))

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
                  :create-link (if *user* (restas:genurl 'edit-article :title title))))

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
                            &key article)
  (apply-template drawer
                  'cliki2.view:view-article-history
                  :title (format nil "History of page \"~A\"" (article-title article))
                  :revisions (revision-summary-list (article-revisions article))
                  :links (article-action-list article :history)))

;; article-revision-page

(defmethod render-key-data ((drawer drawer) (type (eql :article-revision-page))
                            &key article revision)
  (apply-template drawer
                  'cliki2.view:view-article
                  :title (article-title article)
                  :content (generate-html-from-markup (revision-content revision))
                  :links (article-action-list article :revision)))

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
                  :content (generate-html-from-markup (user-info person))
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

;; recent-changes-page

(defmethod render-key-data ((drawer drawer) (type (eql :recent-changes-page))
                            &key revisions)
  (apply-template drawer
                  'cliki2.view:recent-changes
                  :revisions revisions))

;; search-page

(defun article-short-info (article)
  (list :href (restas:genurl 'view-article
                             :title (article-title article))
        :title (article-title article)
        :labels (mapcar #'string-downcase (article-category-list article))
        :changed (hunchentoot:rfc-1123-date (revision-date (article-latest-revision article)))))

(defmethod render-key-data ((drawer drawer) (type (eql :search-page))
                            &key query start articles total)
  (flet ((url (first)
           (format nil
                   "~A?query=~A&start=~A"
                   (restas:genurl 'search-page)
                   (closure-template:encode-uri-component query)
                   first)))
    (apply-template drawer
                    'cliki2.view:search-results
                    :query query
                    :start start
                    :total total
                    :href-after (if (> start 0)
                                    (url (max (- start *search-page-number-results*) 0)))
                    :href-before (if (> (- total start) *search-page-number-results*)
                                     (url (+ start *search-page-number-results*)))
                    :articles (iter (for (article . score) in articles)
                                    (collect (list* :score score
                                                    (article-short-info article)))))))

;; forbidden

(defmethod restas:render-object ((drawer drawer) (code (eql hunchentoot:+http-forbidden+)))
  (apply-template drawer
                  'cliki2.view:forbidden
                  :uri (hunchentoot:request-uri*)))

;; internal server error

(defmethod restas:render-object ((drawer drawer) (code (eql hunchentoot:+http-internal-server-error+)))
  (apply-template drawer
                  'cliki2.view:internal-server-error))

