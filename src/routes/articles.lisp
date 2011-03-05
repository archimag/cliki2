;;;; articles.lisp

(in-package #:cliki2)

(restas:define-route view-article (":title")
  (or (article-with-title title)
      (make-instance 'article-not-found
                     :title title)))

(restas:define-route view-article-source ("raw/:title"
                                          :content-type "text/plain")
  (let ((article (article-with-title title)))
    (unless article
      (restas:abort-route-handler hunchentoot:+http-not-found+))
    (article-content article)))

(restas:define-route edit-article ("edit/:title"
                                   :requirement 'sign-in-p)
  (make-instance 'edit-article-page
                 :title title
                 :article (article-with-title title)))

(restas:define-route forbidden-edit-article ("edit/:title"
                                             :requirement 'not-sign-in-p)
  (declare (ignore title))
  hunchentoot:+http-forbidden+)


(restas:define-route forbidden-save-article ("edit/:title"
                                             :method :post
                                             :requirement 'not-sign-in-p)
  (declare (ignore title))
  hunchentoot:+http-forbidden+)

(defun check-edit-command (field)
  (and (sign-in-p)
       (hunchentoot:post-parameter field)))

(restas:define-route save-article ("edit/:title"
                                   :method :post
                                   :requirement (lambda () (check-edit-command "save")))
  (with-transaction ()
    (let ((article (or (article-with-title title)
                       (make-instance 'article :title title))))
      (push (make-instance 'revision
                           :content (hunchentoot:post-parameter "content")
                           :author *user*
                           :author-ip (hunchentoot:real-remote-addr))
            (article-revisions article))))
  (restas:redirect 'view-article
                   :title title))


(restas:define-route preview-article ("edit/:title"
                                      :method :post
                                      :requirement (lambda () (check-edit-command "preview")))
  (make-instance 'preview-article-page
                 :title title
                 :content (hunchentoot:post-parameter "content")))


(restas:define-route cancel-edit-article ("edit/:title"
                                          :method :post
                                          :requirement (lambda () (check-edit-command "cancel")))
  (restas:redirect 'view-article
                   :title title))




(restas:define-route view-article-history ("history/:(title)")
  (let ((article (article-with-title title)))
    (unless article
      (restas:abort-route-handler hunchentoot:+http-not-found+))
    (make-instance 'article-history-page :article article)))

(restas:define-route view-article-revision ("history/:title/:mark")
  (let ((article (article-with-title title)))
    (render-article-revision *default-render-method*
                             article
                             (find mark
                                   (article-revisions article)
                                   :key #'revision-content-sha1
                                   :test #'string=)
                             :revision)))
