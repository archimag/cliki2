;;;; entry.lisp

(in-package #:cliki2)

(restas:define-route entry ("")
  (view-article :title "index"))

(restas:mount-submodule -static- (#:restas.directory-publisher)
  (restas.directory-publisher:*directory* (merge-pathnames "static/"
                                                           *basepath*))
  (restas.directory-publisher:*autoindex* t))
