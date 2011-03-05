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
               (:file "model" :depends-on ("defmodule"))
               (:file "markup" :depends-on ("defmodule"))
               (:file "render" :depends-on ("model" "markup"))
               (:file "sendmail" :depends-on ("defmodule"))
               (:module "routes"
                        :components
                        ((:file "entry")
                         (:file "articles")
                         (:file "auth"))
                        :depends-on ("render" "sendmail"))))))
