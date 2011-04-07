;;;; cliki2.asd

(asdf:defsystem :cliki2
    :depends-on (#:restas-directory-publisher
                 #:bordeaux-threads
                 #:external-program
                 #:bknr.datastore
                 #:ironclad
                 #:colorize
                 #:cl-recaptcha
                 #:3bmd
                 #:montezuma
                 #:sanitize
                 #:com.gigamonkeys.prose-diff)
    :components
    ((:module "src"
              :components
              ((:file "defmodule")
               (:file "util" :depends-on ("defmodule"))
               (:file "model" :depends-on ("defmodule" "util"))
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
                         (:file "specials")
                         (:file "search"))
                        :depends-on ("view" "auth-core"))))))
