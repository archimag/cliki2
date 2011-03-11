(asdf:defsystem :cliki-convert
  :components ((:module "cliki-convert"
                        :components ((:file "cliki-convert"))))
  :depends-on (#:cliki2 #:cl-fad #:external-program #:closure-template))
