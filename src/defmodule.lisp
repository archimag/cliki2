;;;; defmodule.lisp

(restas:define-module #:cliki2
  (:use #:cl #:iter #:bknr.datastore #:bknr.indices))

(in-package #:cliki2)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defparameter *basepath*
    (make-pathname :directory (pathname-directory (asdf:component-pathname (asdf:find-system '#:cliki2))))))

(defvar *user* nil)

;;;; initialization

(defparameter *datadir* #P"/var/cliki2/")

(defmethod restas:initialize-module-instance ((module (eql #.*package*)) context)
  (let ((*store* nil))
    (restas:with-context context
      (open-store (merge-pathnames "store/" *datadir*))
      (restas:context-add-variable context '*store* *store*)
      (restas:context-add-variable context
                                   '*user*
                                   (or (user-with-name "archimag")
                                       (with-transaction ()
                                           (make-instance 'user :name "archimag" :password "123")))))))

;;;; compile templates

(defun compile-all-templates ()
  (closure-template:compile-template :common-lisp-backend
                                     (merge-pathnames "templates/cliki2.tmpl" *basepath*)))

(compile-all-templates)
