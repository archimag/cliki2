;;;; markup.lisp

(in-package #:cliki2.markup)

(defvar *cliki2-rules* (alexandria:copy-hash-table *rules*))

(defvar *cliki2-compiled-grammar* nil)

(defmacro with-cliki2-rules (&body body)
  `(let ((*rules* *cliki2-rules*))
     ,@body))

(defun recompile-cliki2-grammar ()
  (setf *cliki2-compiled-grammar*
        (esrap:compile-grammar '3bmd-grammar::block)))

(defmacro define-rule (symbol expression &body options)
  `(with-cliki2-rules
     (defrule ,symbol ,expression ,@options)))

(defun parse-cliki2-doc (markup &aux (curpos 0))
  (with-cliki2-rules
    (unless *cliki2-compiled-grammar*
      (recompile-cliki2-grammar))
    (iter (multiple-value-bind (block pos)
              (parse *cliki2-compiled-grammar* markup :start curpos :junk-allowed t)
            (while block)
            (collect block)
            (while pos)
            (setf curpos pos)))))

(defun generate-html-from-markup (markup)
  (let ((input (3bmd::expand-tabs markup :add-newlines t)))
    (sanitize:clean (with-output-to-string (s)
                      (3bmd:print-doc-to-stream (parse-cliki2-doc input) s))
                    sanitize:+relaxed+)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cliki2 markup extensions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-rule article-link (and (and (? #\\) "_(") (+ (and (! #\)) character)) #\))
  (:destructure (start article end)
    (declare (ignore start end))
    (cons :article-link (cliki2:normalize-name (concat article)))))

(defmethod 3bmd:print-tagged-element ((tag (eql :article-link)) stream title)
  (write-string (cliki2.view:article-link
                 (list :title title
                       :href (restas:genurl 'cliki2:view-article :title title)))
                stream))

(define-rule person-link (and "_P(" (+ (and (! #\)) character)) #\))
  (:destructure (start name end)
    (declare (ignore start end))
    (cons :person-link (cliki2:normalize-name (concat name)))))

(defmethod 3bmd:print-tagged-element ((tag (eql :person-link)) stream name)
  (write-string (cliki2.view:person-link
                 (list :name name
                       :href (restas:genurl 'cliki2:view-person :name name)))
                stream))

(define-rule hyperspec-link (and "_H(" (+ (and (! #\)) character)) #\))
  (:destructure (start symbol end)
    (declare (ignore start end))
    (cons :hyperspec-link (concat symbol))))

(defmethod 3bmd:print-tagged-element ((tag (eql :hyperspec-link)) stream symbol)
  (write-string (cliki2.view:hyperspec-link
                 (list :symbol symbol
                       :href (clhs-lookup:spec-lookup symbol)))
                stream))

(define-rule category-link (and (and (? #\\) "*(") (+ (and (! #\)) character)) #\))
  (:destructure (start category end)
    (declare (ignore start end))
    (cons :article-link (cliki2:normalize-name (concat category)))))

(define-rule empty-lines
    (* (and (* (or #\Space #\Tab)) (? #\Return) #\Newline)))

(define-rule code-block (and "<code>"
                             empty-lines
                             (+ (and (! (and empty-lines "</code>")) character))
                             empty-lines
                             "</code>")
  (:destructure (start w1 code w2 end)
    (declare (ignore start w1 w2 end))
    (cons :lisp-code-block (concat code))))

(defmethod 3bmd:print-tagged-element ((tag (eql :lisp-code-block)) stream code)
  (write-string (cliki2.view:code-block
                 (list :code (colorize::html-colorization :common-lisp code)))
                stream))
  
(defun category-char-p (character)
  (not (member character '(#\: #\" #\)))))

(define-rule category-name (and (? #\") (+ (category-char-p character)) (? #\"))
  (:lambda (list)
    (concat (second list))))

(define-rule category-list (and (and (? #\\) "_/(")
                                category-name
                                (* (and (! #\)) character))                                
                                ")")
  (:lambda (list)
    (cons :cliki2-category-list (cliki2:category-keyword (second list)))))

(sanitize:define-sanitize-mode +simple+
    :elements ("a")
    :attributes (("a" . ("href")))
    :protocols  (("a" . (("href" . (:ftp :http :https :mailto :relative))))))

(defun format-article-description (article)
  (sanitize:with-clean-fragment (fragment
                                 (with-output-to-string (s)
                                   (let ((3bmd::*references* (make-hash-table)))
                                     (3bmd::print-element (parse '3bmd-grammar::block  
                                                                 (cliki2::article-content-head article)
                                                                 :junk-allowed t)
                                                          s)))
                                 +simple+)
    (with-output-to-string (out)
      (iter (for item in-child-nodes fragment)
            (for text = (html:serialize-html item :to-string))
            (for len initially 0 then (+ len (length text)))
            (let ((dot-pos (if (xtree:text-p item) (position #\. text))))
              (cond
                (dot-pos (write-string (subseq text 0 (1+ dot-pos)) out)
                         (finish))
                (t (write-string text out))))))))

(defmethod 3bmd:print-tagged-element ((tag (eql :cliki2-category-list)) stream category)
  (write-string (cliki2.view:category-content
                 (list :items
                       (iter (for article in (sort (copy-list (cliki2::articles-with-category category))
                                                   #'string<
                                                   :key 'cliki2::article-downcase-title))
                             (collect
                                 (list :title (cliki2::article-title article)
                                       :head (format-article-description article)
                                       :href (restas:genurl 'cliki2:view-article
                                                            :title (cliki2::article-title article)))))))
                stream))


(define-rule 3bmd-grammar:inline-extensions
    (or article-link
        person-link
        hyperspec-link
        category-link
        code-block
        category-list))
