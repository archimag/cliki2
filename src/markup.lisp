;;;; markup.lisp

(in-package #:cliki2)


(defun generate-html-from-markup (markup)
  (let ((doc (docutils:read-rst (ppcre:regex-replace-all "\\r\\n" markup (string #\Newline) )))
        (writer (make-instance 'docutils.writer.html:html-writer)))
    (docutils:visit-node writer doc)
    (with-output-to-string (out)
      (iter (for part in  '(docutils.writer.html:body-pre-docinfo 
                            docutils.writer.html:docinfo
                            docutils.writer.html:body))
            (docutils:write-part writer part out))
      (format out "</div>"))))
