;;;; articles.lisp

(in-package #:cliki2)

(restas:define-route view-article (":title")
  (or (let ((article (article-with-title title)))
        (when (and article
                   (not (string= (article-title article) title)))
          (restas:redirect 'view-article
                           :title (article-title article)))
        article)
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
  (add-revision (or (article-with-title title)
                    (make-instance 'article :title title))
                (hunchentoot:post-parameter "summary")
                (hunchentoot:post-parameter "content"))
  (restas:redirect 'view-article :title title))

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

(defun find-article-revision (article date)
  (find date
        (article-revisions article)
        :key #'revision-date))

(restas:define-route compare-article-revisions ("history/:(title)"
                                                :method :post)
  (flet ((get-revision (article href)
           (find-article-revision article
                                  (getf (restas:parse-route-url href 'view-article-revision)
                                        :date))))
    (let* ((article (check-article title))
           (new (get-revision article (hunchentoot:post-parameter "diff")))
           (old (get-revision article (hunchentoot:post-parameter "old"))))
      (list :revisions-diff-page
            :article article
            :new (closure-template:escape-html (revision-content new))
            :old (closure-template:escape-html (revision-content old))))))
          

(restas:define-route view-article-revision ("history/:title/:date"
                                            :parse-vars (list :date #'parse-integer))
  (let ((article (check-article title)))
    (list :article-revision-page
          :article article
          :revision (find-article-revision article date))))

