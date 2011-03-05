;;;; cliki2.asd

(defsystem cliki2
    :depends-on (#:restas-directory-publisher #:bknr.datastore #:ironclad #:docutils)
    :components
    ((:module "src"
              :components
              ((:file "defmodule")
               (:file "model" :depends-on ("defmodule"))
               (:file "render" :depends-on ("model"))
               (:file "routes" :depends-on ("render"))))))
