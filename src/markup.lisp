;;;; markup.lisp

(in-package #:cliki2)

(defun generate-html-from-markup (markup)
  (with-output-to-string (s)
    (3bmd:parse-string-and-print-to-stream markup s)))

;;;; article-ref (_())


;;;; person-ref (_())

;;;; hypespec-ref ???

;;;; code-block <code></code>

;; (defmethod docutils:visit-node ((writer docutils.writer.html:html-writer) (node code-block))
;;   (let ((lang (car (assoc (code-block-lang node)
;;                           (colorize:coloring-types)
;;                           :test #'string-equal))))
;;     (if lang
;;         (append-template 'cliki2.view:code-block
;;                          :code (colorize::html-colorization :common-lisp
;;                                                             (code-block-code node)))
;;         (docutils:part-append
;;          (format nil "<pre>~A</pre>" (code-block-code node))))))

;;;; category *()

;;;; category list /()

;; (defmethod docutils:visit-node ((writer docutils.writer.html:html-writer) (node category-content))
;;   (append-template 'cliki2.view:category-content
;;                    :items (iter (for article in (articles-with-category (category-content-title node)))
;;                                 (collect
;;                                     (list :title (article-title article)
;;                                           :href (restas:genurl 'view-article
;;                                                                :title (article-title article)))))))


