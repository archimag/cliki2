;;;; cliki2.asd

(asdf:defsystem :cliki2
    :depends-on (#:restas-directory-publisher
                 #:bordeaux-threads
                 #:bknr.datastore
                 #:ironclad
                 #:colorize
                 #:cl-recaptcha
                 #:3bmd)
    :components
    ((:module "src"
              :components
              ((:file "defmodule")
               (:file "sendmail" :depends-on ("defmodule"))
               (:file "model" :depends-on ("defmodule"))
               (:file "categories" :depends-on ("defmodule"))
               (:file "markup" :depends-on ("categories" "defmodule"))
               (:file "view" :depends-on ("model" "markup"))
               (:file "auth-core" :depends-on ("model"))
               (:module "routes"
                        :components
                        ((:file "entry")
                         (:file "check")
                         (:file "articles" :depends-on ("check"))
                         (:file "person" :depends-on ("check"))
                         (:file "auth")
                         (:file "specials"))
                        :depends-on ("view" "sendmail" "auth-core"))))))
