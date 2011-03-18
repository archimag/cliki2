(asdf:defsystem :cliki-convert
  :components ((:module "cliki-convert"
                        :components ((:file "artefacts")
                                     (:file "cliki-convert" :depends-on ("artefacts")))))
  :depends-on (#:cliki2 #:cl-fad #:external-program))
