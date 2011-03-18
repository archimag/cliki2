;;;; defmodule.lisp

(defpackage #:cliki2.categories)

(defpackage #:cliki2.markup
  (:use #:cl #:iter #:esrap)
  (:export #:generate-html-from-markup))

(restas:define-module #:cliki2
  (:use #:cl #:iter #:bknr.datastore #:bknr.indices #:cliki2.markup)
  (:export #:view-article #:view-person
           #:category-keyword))

(in-package #:cliki2)

;;;; base path

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defparameter *basepath*
    (make-pathname :directory (pathname-directory (asdf:component-pathname (asdf:find-system '#:cliki2))))))

;;; mails

(defparameter *sendmail*
  (find-if #'fad:file-exists-p
           '("/usr/bin/sendmail" "/usr/sbin/sendmail")))

(defparameter *noreply-email* "noreply@cliki2.net")

;;;; auth

(defvar *user* nil)

(defun sign-in-p ()
  *user*)

(defun not-sign-in-p ()
  (not *user*))

(defparameter *reCAPTCHA.publick-key* "6LdZjAcAAAAAAGh_MzHcHfJWp6rpI0XUNghGQB1f")

(defparameter *reCAPTCHA.privake-key* "6LdZjAcAAAAAAKJ2GPWTHPh1H1Foc0kyfbwgrFgO")

(defparameter *reCAPTCHA.theme* nil)

(defparameter *cookie-auth-name* "userauth")

(defparameter *cookie-cipher-key* (ironclad:ascii-string-to-byte-array "Specify the secure key"))

(defvar *user-auth-cipher*)

;;;; store

(defparameter *datadir* #P"/var/cliki2/")

;;;; initialization

(defmethod restas:initialize-module-instance ((module (eql #.*package*)) context)
  (unless (restas:context-symbol-value context '*default-render-method*)
    (restas:context-add-variable context
                                 '*default-render-method*
                                 (make-instance 'drawer)))

  (restas:with-context context
    (setf (restas:context-symbol-value context '*user-auth-cipher*)
          (ironclad:make-cipher :blowfish 
                                :mode :ecb
                                :key (restas:context-symbol-value context '*cookie-cipher-key*)))

    (let ((*store* nil))
      (open-store (merge-pathnames "store/" *datadir*))
      (setf (restas:context-symbol-value context '*store*)
            *store*))))

;;;; compile templates

(defun compile-all-templates ()
  (closure-template:compile-template :common-lisp-backend
                                     (cl-fad:list-directory (merge-pathnames "templates/" *basepath*))))

(compile-all-templates)

;;;; decorators

(push '@check-auth-user *decorators*)