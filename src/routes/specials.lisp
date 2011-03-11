;;;; entry.lisp

(in-package #:cliki2)

(defgeneric object-link-info (object)
  (:method ((article article))
    (list :href (restas:genurl 'view-article
                               :title (article-title article))
          :title (article-title article)))
  (:method ((person user))
    (list :href (restas:genurl 'view-person
                               :name (user-name person))
          :title (user-name person))))

(restas:define-route recent-changes ("specials/recent-changes")
  (list :recent-changes-page
        :revisions (revision-summary-list (get-recent-revisions))))


(restas:define-route recent-changes-feed ("feed/rss.xml"
                                          :content-type "application/rss+xml"
                                          :render-method 'cliki2.view:rss-feed)
  (list* :title "CLiki Recent Changes"
        :link (restas:gen-full-url 'recent-changes-feed)
        (cdr (recent-changes))))
