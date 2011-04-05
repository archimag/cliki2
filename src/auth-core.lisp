;;;; auth-core.lisp

(in-package #:cliki2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; core
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pack-auth-cookie (name password &key (version 1) (date (get-universal-time)))
  (format nil "~A|~A|~A|~A" version name password date))

(defun encrypt-auth-cookie (name password &key (version 1) (date (get-universal-time)))
  (let ((result (ironclad:ascii-string-to-byte-array
                 (pack-auth-cookie name password :version version :date date))))
    (ironclad:encrypt-in-place *user-auth-cipher* result)
    (ironclad:byte-array-to-hex-string result)))

(defun set-auth-cookie (name password &key (version 1))
  (hunchentoot:set-cookie *cookie-auth-name*
                          :value (encrypt-auth-cookie name password :version version)
                          :path "/"
                          :expires (+ (get-universal-time) (* 60 60 24 4))
                          :http-only t))

;;;; get-auth-cookie

(defun unpack-auth-cookie (str)
  (let ((info (split-sequence:split-sequence #\| str)))
    (values (first info)
            (second info)
            (third info)
            (fourth info))))

(defun hex-string-to-byte-array (string &key (start 0) (end nil))
  (declare (type string string))
  (let* ((end (or end (length string)))
         (length (/ (- end start) 2))
         (key (make-array length :element-type '(unsigned-byte 8))))
    (declare (type (simple-array (unsigned-byte 8) (*)) key))
    (flet ((char-to-digit (char)
             (let ((x (position char "0123456789abcdef" :test #'char-equal)))
               (or x (error "Invalid hex key ~A specified" string)))))
      (loop for i from 0
            for j from start below end by 2
            do (setf (aref key i)
                     (+ (* (char-to-digit (char string j)) 16)
                        (char-to-digit (char string (1+ j)))))
         finally (return key)))))

(defun decrypt-auth-cookie (str)
  (ignore-errors
    (let ((result (hex-string-to-byte-array str)))
      (ironclad:decrypt-in-place *user-auth-cipher*
                                 result)
      (unpack-auth-cookie (babel:octets-to-string result :encoding :utf-8)))))

(defun get-auth-cookie ()
  (let ((cookie (hunchentoot:cookie-in *cookie-auth-name*)))
    (if cookie
        (decrypt-auth-cookie cookie))))


(defun run-sing-in (user &key (version 1))
  "Set cookie for user name and password"
  (setf *user* user)
  (set-auth-cookie (user-name user)
                   (user-password-digest user)
                   :version version))

(defun run-sign-out ()
  "Clear cookie with auth information"
  (setf *user* nil)
  (hunchentoot:set-cookie *cookie-auth-name*
                          :value ""
                          :path "/"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; @check-auth-user decorator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *kdf* (ironclad:make-kdf 'ironclad:pbkdf2 :digest 'ironclad:sha256))

(defun password-digest (password salt)
  (ironclad:byte-array-to-hex-string
   (ironclad:derive-key *kdf*
                        (babel:string-to-octets password :encoding :utf-8)
                        salt
                        1000 128)))

(defun make-random-salt ()
  (let ((salt (make-array 50 :element-type 'ub8)))
    (dotimes (i (length salt))
      (setf (aref salt i) (random 256)))
    salt))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; @check-auth-user decorator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass check-auth-user-route (routes:proxy-route) ())

(defun check-user-auth ()
  (multiple-value-bind (version name password date) (get-auth-cookie)
    (if (and version name password date)
        (let ((user (user-with-name name)))
          (if (and user
                   (string= (user-password-digest user)
                            password)
                   (or (null (user-role user))
                       (member (user-role user) '(:moderator :administrator))))
              user)))))
        
(defmethod routes:route-check-conditions ((route check-auth-user-route) bindings)
  (let ((*user* (check-user-auth)))
    (call-next-method)))

(defmethod restas:process-route ((route check-auth-user-route) bindings)
  (let ((*user* (check-user-auth)))
    (call-next-method)))

(defun @check-auth-user (origin)
  (make-instance 'check-auth-user-route :target origin))

