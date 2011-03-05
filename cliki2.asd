;;;; cliki2.asd

(defsystem cliki2
    :depends-on (#:restas-directory-publisher
                 #:bknr.datastore
                 #:ironclad
                 #:docutils
                 #:cl-recaptcha)
    :components
    ((:module "src"
              :components
              ((:file "defmodule")
               (:file "sendmail" :depends-on ("defmodule"))
               (:file "model" :depends-on ("defmodule"))
               (:file "markup" :depends-on ("defmodule"))
               (:file "view" :depends-on ("model" "markup"))
               (:file "auth-core" :depends-on ("model"))
               (:module "routes"
                        :components
                        ((:file "entry")
                         (:file "articles")
                         (:file "person")
                         (:file "auth"))
                        :depends-on ("view" "sendmail" "auth-core"))))))
