;;;; check.lisp

(in-package #:cliki2)

(defmacro check-edit-command (field)
  `(lambda ()
     (and (sign-in-p)
          (hunchentoot:post-parameter ,field))))

(defun check-article (title &aux (article (article-with-title title)))
  (unless article
    (restas:abort-route-handler hunchentoot:+http-not-found+))
  article)
  
(defun check-article-edit-access ()
  (unless (sign-in-p)
    (restas:abort-route-handler hunchentoot:+http-forbidden+)))

(defun check-person (name &aux (person (user-with-name name)))
  (unless person
    (restas:abort-route-handler hunchentoot:+http-not-found+))
  person)

(defun check-owner-person (name &aux (person (check-person name)))
  (unless (and *user*
               (string= (user-name *user*)
                        (user-name person)))
    (restas:abort-route-handler hunchentoot:+http-forbidden+))
  person)

(defun check-intive (mark &aux (invite (invite-with-mark mark)))
  (unless invite
    (restas:abort-route-handler hunchentoot:+http-not-found+))
  invite)
