;;;; render.lisp

(in-package #:cliki2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; drawer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass drawer () ())

(setf *default-render-method* (make-instance 'drawer))

(defgeneric render-article-revision (drawer article revision mode))

(defgeneric render-handle-markup (drawer content))


(defmethod render-handle-markup ((drawer drawer) content)  
  (let ((doc (docutils:read-rst (ppcre:regex-replace-all "\\r\\n" content (string #\Newline) )))
        (writer (make-instance 'docutils.writer.html:html-writer)))
    (docutils:visit-node writer doc)
    (with-output-to-string (out)
      (iter (for part in  '(docutils.writer.html:body-pre-docinfo 
                            docutils.writer.html:docinfo
                            docutils.writer.html:body))
            (docutils:write-part writer part out))
      (format out "</div>"))))

(defun article-action-list (article mode
                            &aux (title (article-title article)))
  (case mode
    (:view (list :edit (restas:genurl 'edit-article :title title)
                 :raw (restas:genurl 'view-article-source :title title)
                 :history (restas:genurl 'view-article-history :title title)))
    
    (:history (list :view (restas:genurl 'view-article :title title)))

    (:revision (list :view (restas:genurl 'view-article :title title)
                     :history (restas:genurl 'view-article-history :title title)))))

(defmethod render-article-revision ((drawer drawer) (article article) (revision revision) mode)
  (let ((title (article-title article)))
    (cliki2.view:view-article
     (list :title title
           :content (render-handle-markup drawer (revision-content revision))
           :links (article-action-list article mode)))))

(defmethod restas:render-object ((drawer drawer) (article article))
  (render-article-revision drawer article (article-last-revision article) :view))

