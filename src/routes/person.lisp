;;;; auth.lisp

(in-package #:cliki2)

(restas:define-route view-person ("person/:name")
  (check-person name))
    
(restas:define-route edit-person ("person/edit/:name")
  (list :edit-person-page
        :person (check-owner-person name)))

(restas:define-route save-person ("person/edit/:name"
                                  :method :post
                                  :requirement (check-edit-command "save"))
  (let ((person (check-owner-person name)))
    (setf (user-info person)
          (hunchentoot:post-parameter "content"))
    (restas:redirect 'view-person :name name)))

(restas:define-route preview-person ("person/edit/:name"
                                     :method :post
                                     :requirement (check-edit-command "preview"))
  (list :preview-person-page
        :person (check-owner-person name)
        :info (hunchentoot:post-parameter "content")))

(restas:define-route cancel-edit-person ("person/edit/:name"
                                          :method :post
                                          :requirement (check-edit-command "cancel"))
  (check-owner-person name)
  (restas:redirect 'view-person
                   :name name))
