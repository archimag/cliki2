;;;; auth.lisp

(in-package #:cliki2)

(restas:define-route sign-out ("specials/singout")
  (run-sign-out)
  (restas:redirect (hunchentoot:referer)))


(restas:define-route sign-in ("specials/singin")
  :sign-in-page)

(restas:define-route sign-in/post ("specials/singin"
                                   :method :post
                                   :requirement 'not-sign-in-p)
  (let* ((name (hunchentoot:post-parameter "name"))
         (user (user-with-name name))
         (password (password-digest (hunchentoot:post-parameter "password")
                                    (user-password-salt user)))
         (done (hunchentoot:get-parameter "done")))
    (cond
      ((and user (string= (user-password-digest user) password))
       (run-sing-in user)
       (restas:redirect (or done "/")))
      (t (restas:redirect 'sign-in)))))
       
(restas:define-route register ("specials/register")
  (list :register-page :data nil))

(defun form-field-value (field)
  (hunchentoot:post-parameter field))

(defun form-field-empty-p (field)
  (string= (form-field-value field)
           ""))

(defparameter *re-email-check* 
  "^[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*@(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?$")

(defun check-register-form ()
  (let ((bads nil))
    (flet ((form-error-message (field message)
             (push message bads)
             (push field bads)))
      (cond
        ((form-field-empty-p "name")
         (form-error-message :bad-name "empty"))
        ((user-with-name (form-field-value "name"))
         (form-error-message :bad-name "exist")))
      
      (cond
        ((form-field-empty-p "email") (form-error-message :bad-email "empty"))
        ((not (ppcre:scan *re-email-check*
                          (string-downcase (form-field-value "email"))))
         (form-error-message :bad-email
                             "bad"))
        ((user-with-email (form-field-value "email"))
         (form-error-message :bad-email
                             "exist")))

      (cond
        ((form-field-empty-p "password")
         (form-error-message :bad-password
                             "empty"))
        ((< (length (form-field-value "password")) 8)
         (form-error-message :bad-password
                             "short")))
      
      (unless (string= (form-field-value "password")
                       (form-field-value "re-password"))
        (form-error-message :bad-re-password
                            "bad"))

      (unless (cl-recaptcha:verify-captcha (hunchentoot:post-parameter "recaptcha_challenge_field")
                                           (hunchentoot:post-parameter "recaptcha_response_field")
                                           (hunchentoot:real-remote-addr)
                                           :private-key *reCAPTCHA.privake-key*)
        (form-error-message :bad-recaptcha "Bad")))
    
      bads))

(restas:define-route register/post ("specials/register"
                                    :method :post
                                    :requirement 'not-sign-in-p)
  (let ((fails (check-register-form))
        (nickname (form-field-value "name"))
        (email (form-field-value "email"))
        (password (form-field-value "password")))
    (cond
      (fails
       (list :register-page
             :data (list* :name nickname
                          :email email
                          :password password
                          :re-password (form-field-value "re-password")
                          fails)))
      (t (let ((invite
                (let* ((salt (make-random-salt))
                       (digest (password-digest password salt)))
                  (with-transaction ()
                    (make-instance 'invite
                                   :user (make-instance
                                          'user
                                          :name nickname
                                          :email email
                                          :password-salt salt
                                          :password-digest digest
                                          :role :invite)))))
               (to (list email)))
           (sendmail to
                     (cliki2.view:confirmation-mail
                      (list :to to
                            :noreply-mail *noreply-email*
                            :subject (prepare-subject "Потверждение регистрации")
                            :host (hunchentoot:host)
                            :link (restas:gen-full-url 'confirm-registration
                                                       :mark (invite-mark invite)))))
           :register-sendmail-page)))))

(restas:define-route confirm-registration ("specials/invite/:mark"
                                           :requirement 'not-sign-in-p)
  (check-intive mark)
  :confirm-registration-page)

(restas:define-route confirm-registration/post ("specials/invite/:mark"
                                                :method :post
                                                :requirement 'not-sign-in-p)
  (let ((invite (check-intive mark)))
    (with-transaction ()
      (setf (user-role (invite-user invite))
            nil)
      (delete-object invite))
    
    (restas:redirect 'entry)))
   