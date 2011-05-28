(asdf:defsystem :cliki-convert
  :components ((:module "cliki-convert"
                        :components ((:file "package")
                                     (:file "artefacts" :depends-on ("package"))
                                     (:file "cliki-convert" :depends-on ("package" "artefacts")))))
  :depends-on (#:cliki2 #:cl-fad #:external-program))
