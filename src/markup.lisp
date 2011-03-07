;;;; markup.lisp

(in-package #:cliki2)

(defvar *interpreted-roles*
  (alexandria:copy-hash-table docutils.parser.rst::*interpreted-roles*))

(defvar *directives*
  (alexandria:copy-hash-table docutils.parser.rst::*directives*))

(defmacro with-cliki2-markup (&body body)
  `(let ((*interpreted-roles* docutils.parser.rst::*interpreted-roles*)
         (*directives* docutils.parser.rst::*directives*))
     ,@body))


(defun generate-html-from-markup (markup)
  (with-cliki2-markup
    (let ((doc (docutils:read-rst (ppcre:regex-replace-all "\\r\\n" markup (string #\Newline) )))
          (writer (make-instance 'docutils.writer.html:html-writer)))
      (docutils:visit-node writer doc)
      (with-output-to-string (out)
        (iter (for part in  '(docutils.writer.html:body-pre-docinfo 
                              docutils.writer.html:docinfo
                              docutils.writer.html:body))
              (docutils:write-part writer part out))
        (format out "</div>")))))

(defun append-template (template &rest args &key &allow-other-keys)
  (docutils:part-append
   (funcall template args)))

;;;; article-ref

(defclass article-ref (docutils.nodes:raw)
  ((title :initarg :title :reader article-ref-title)))

(defmethod docutils:visit-node ((write docutils.writer.html:html-writer) (node article-ref)
                                &aux (title (article-ref-title node)))
  (append-template 'cliki2.view:article-link
                   :title title
                   :href (restas:genurl 'view-article
                                        :title title)))
(with-cliki2-markup
  (docutils.parser.rst:def-role article (title)
    (make-instance 'article-ref
                   :title title)))

;;;; person-ref

(defclass person-ref (docutils.nodes:raw)
  ((name :initarg :name :reader person-ref-name)))

(defmethod docutils:visit-node ((write docutils.writer.html:html-writer) (node person-ref)
                                &aux (name (person-ref-name node)))
  (append-template 'cliki2.view:person-link
                   :name name
                   :href (restas:genurl 'view-person
                                        :name name)))
(with-cliki2-markup
  (docutils.parser.rst:def-role person (name)
    (make-instance 'person-ref
                   :name name)))

;;;; hypespec-ref

(defclass hyperspec-ref (docutils.nodes:raw)
  ((symbol :initarg :symbol :reader hyperspec-ref-symbol)))

(defmethod docutils:visit-node ((write docutils.writer.html:html-writer) (node hyperspec-ref)
                                &aux (symbol (hyperspec-ref-symbol node)))
  (append-template 'cliki2.view:hyperspec-link
                   :symbol symbol
                   :href (clhs-lookup:spec-lookup (hyperspec-ref-symbol node))))

(with-cliki2-markup
  (docutils.parser.rst:def-role hs (symbol)
    (make-instance 'hyperspec-ref
                   :symbol symbol)))

;;;; code-block

(defclass code-block (docutils.nodes:raw)
  ((lang :initarg :lang :initform nil :reader code-block-lang)
   (code :initarg :code :initform nil :reader code-block-code)))

(defmethod docutils:visit-node ((writer docutils.writer.html:html-writer) (node code-block))
  (let ((lang (car (assoc (code-block-lang node)
                          (colorize:coloring-types)
                          :test #'string-equal))))
    (if lang
        (append-template 'cliki2.view:code-block
                         :code (colorize::html-colorization :common-lisp
                                                            (code-block-code node)))
        (docutils:part-append
         (format nil "<pre>~A</pre>" (code-block-code node))))))

(with-cliki2-markup
  (docutils.parser.rst:def-directive code-block (parent lang &content content)
    (let ((node (docutils:make-node 'docutils.nodes:paragraph)))
      (docutils:add-child node
                          (make-instance 'code-block
                                         :lang lang
                                         :code (docutils::join-strings content #\Newline)))
      (docutils:add-child parent node))))

;;;; category

(defclass category-ref (docutils.nodes:raw)
  ((title :initarg :title :initform nil :reader category-ref-title)))

(defmethod docutils:visit-node ((write docutils.writer.html:html-writer) (node category-ref)
                                &aux (title (category-ref-title node)))
  (append-template 'cliki2.view:category-link
                   :title title
                   :href (restas:genurl 'view-article
                                        :title title)))
(with-cliki2-markup
  (docutils.parser.rst:def-role category (title)
    (make-instance 'category-ref
                   :title title)))

(defun category-keyword (str)
  (intern (ppcre:regex-replace-all "(\\s)+" (string-upcase str) "-")
          :keyword))

(defun content-categories (markup)
  (with-cliki2-markup
    (let ((doc (docutils:read-rst (ppcre:regex-replace-all "\\r\\n" markup (string #\Newline) )))
          (categories nil))
      (docutils:with-nodes (node doc)
        (typecase node
          (category-ref
           (push (category-keyword (category-ref-title node))
                 categories))))
      categories)))

(defclass category-content (docutils.nodes:raw)
  ((title :initarg :title :initform nil :reader category-content-title)))
  
(defmethod docutils:visit-node ((writer docutils.writer.html:html-writer) (node category-content))
  (append-template 'cliki2.view:category-content
                   :items (iter (for article in (articles-with-category (category-content-title node)))
                                (collect
                                    (list :title (article-title article)
                                          :href (restas:genurl 'view-article
                                                               :title (article-title article)))))))
  
(with-cliki2-markup
  (docutils.parser.rst:def-directive category-content (parent title &allow-spaces)
    (let ((node (docutils:make-node 'docutils.nodes:paragraph)))
      (docutils:add-child node
                          (make-instance 'category-content
                                         :title (category-keyword title)))
      (docutils:add-child parent node))))
    