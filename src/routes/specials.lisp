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
        :changes (iter (for item in (cliki2-recent-changes *store*))
                       (collect
                           (destructuring-bind (object user date comment) item
                             (list* :date (hunchentoot:rfc-1123-date date)
                                    :user (list :href (restas:genurl 'view-person
                                                                     :name (user-name user))
                                                :name (user-name user))
                                    :comment comment
                                    (object-link-info object)))))))
