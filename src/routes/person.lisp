;;;; auth.lisp

(in-package #:cliki2)

(defun check-person (person)
  (unless person
    (restas:abort-route-handler hunchentoot:+http-not-found+))
  person)

(defun check-owner-person (person)
  (check-person person)
  (unless (and *user*
               (string= (user-name *user*)
                        (user-name person)))
      (restas:abort-route-handler hunchentoot:+http-forbidden+))
  person)

(restas:define-route view-person ("person/:name")
  (check-person (user-with-name name)))
    
(restas:define-route edit-person ("person/edit/:name"
                                  :requirement 'sign-in-p)
  (make-instance 'edit-person-page
                 :person (check-owner-person (user-with-name name))))


(restas:define-route save-person ("person/edit/:name"
                                  :method :post
                                  :requirement (lambda () (check-edit-command "save")))
  (let ((person (check-owner-person (user-with-name name))))
    (setf (user-info person)
          (hunchentoot:post-parameter "content"))
    (restas:redirect 'view-person :name name)))


(restas:define-route preview-person ("person/edit/:name"
                                      :method :post
                                      :requirement (lambda () (check-edit-command "preview")))
  (let ((person (check-owner-person (user-with-name name))))
    (make-instance 'preview-person-page
                   :person person
                   :info (hunchentoot:post-parameter "content"))))

(restas:define-route cancel-edit-person ("person/edit/:name"
                                          :method :post
                                          :requirement (lambda () (check-edit-command "cancel")))
  (restas:redirect 'view-person
                   :name name))
