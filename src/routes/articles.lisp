;;;; articles.lisp

(in-package #:cliki2)

(restas:define-route view-article (":title")
  (or (article-with-title title)
      (list :article-not-found-page
            :title title)))

(restas:define-route view-article-source ("raw/:title"
                                          :content-type "text/plain")
  (article-content (check-article title)))

(restas:define-route edit-article ("edit/:title")
  (check-article-edit-access)
  (list :edit-article-page
        :title title
        :article (article-with-title title)))

(restas:define-route save-article ("edit/:title"
                                   :method :post
                                   :requirement (check-edit-command "save"))
  (check-article-edit-access)
  (let ((article (or (article-with-title title)
                     (with-transaction ()
                       (make-instance 'article :title title))))
        (content-sha1 (save-content (hunchentoot:post-parameter "content"))))
    (with-transaction ()
      (push (make-instance 'revision
                           :content-sha1 content-sha1
                           :author *user*
                           :author-ip (hunchentoot:real-remote-addr))
            (article-revisions article))))
  (unless (hunchentoot:post-parameter "minoredit")
    (add-change (article-with-title title)
                *user*
                (get-universal-time)
                (hunchentoot:post-parameter "summary")))
  (restas:redirect 'view-article
                   :title title))

(restas:define-route preview-article ("edit/:title"
                                      :method :post
                                      :requirement (check-edit-command "preview"))
  (check-article-edit-access)
  (list :preview-article-page
        :title title
        :content (hunchentoot:post-parameter "content")))


(restas:define-route cancel-edit-article ("edit/:title"
                                          :method :post
                                          :requirement (check-edit-command "cancel"))
  (check-article-edit-access)
  (restas:redirect 'view-article
                   :title title))

(restas:define-route view-article-history ("history/:(title)")
  (list :article-history-page
        :article (check-article title)))

(restas:define-route view-article-revision ("history/:title/:mark")
  (let ((article (check-article title)))
    (list :article-revision-page
          :article article
          :revision (find mark
                          (article-revisions article)
                          :key #'revision-content-sha1
                          :test #'string=))))

