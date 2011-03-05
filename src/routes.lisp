;;;; routes.lisp

(in-package #:cliki2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; entry 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(restas:define-route entry ("")
  (view-article :title "index"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; static
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(restas:mount-submodule -static- (#:restas.directory-publisher)
  (restas.directory-publisher:*directory* (merge-pathnames "static/"
                                                           *basepath*))
  (restas.directory-publisher:*autoindex* t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; article
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(restas:define-route view-article (":title")
  (or (article-with-title title)
      (restas:abort-route-handler
       (cliki2.view:article-not-found
        (list :title title
              :create-link (restas:genurl 'edit-article
                                          :title title))))))

(restas:define-route view-article-source ("raw/:title"
                                          :content-type "text/plain")
  (let ((article (article-with-title title)))
    (unless article
      (restas:abort-route-handler hunchentoot:+http-not-found+))
    (article-content article)))

(restas:define-route edit-article ("edit/:title"
                                   :render-method 'cliki2.view:edit-article)
  (let ((article (article-with-title title)))
    (list :title title
          :content (if article
                       (article-content article)
                       ""))))

(restas:define-route save-article ("edit/:title"
                                   :method :post
                                   :requirement (lambda () (hunchentoot:post-parameter "save")))
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
                                      :render-method 'cliki2.view:edit-article
                                      :requirement (lambda () (hunchentoot:post-parameter "preview")))
  (list :title title
        :content (hunchentoot:post-parameter "content")
        :preview (hunchentoot:post-parameter "content")))

(restas:define-route cancel-edit-article ("edit/:title"
                                          :method :post
                                          :requirement (lambda () (hunchentoot:post-parameter "cancel")))
  (restas:redirect 'view-article
                   :title title))




(restas:define-route view-article-history ("history/:(title)"
                                           :render-method 'cliki2.view:view-article-history)
  (let ((article (article-with-title title)))
    (unless article
      (restas:abort-route-handler hunchentoot:+http-not-found+))

    (list :title (format nil "History of page \"~A\"" title)
          :history (iter (for revision in (article-revisions article))
                         (collect
                             (list :href (restas:genurl 'view-article-revision
                                                         :title title
                                                         :mark (revision-content-sha1 revision))
                                    :author (user-name (revision-author revision))
                                    :date (hunchentoot:rfc-1123-date (revision-date revision)))))
          :links (article-action-list article :history))))

(restas:define-route view-article-revision ("history/:title/:mark")
  (let ((article (article-with-title title)))
    (render-article-revision *default-render-method*
                             article
                             (find mark
                                   (article-revisions article)
                                   :key #'revision-content-sha1
                                   :test #'string=)
                             :revision)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; person article
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


