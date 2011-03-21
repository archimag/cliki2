;;;; search.lisp

(in-package #:cliki2)

(defun search-articles (query &optional (start 0))
  (let ((docs (montezuma:search *search-index*
                                (format nil 
                                        "content:\"~A\""
                                        (remove #\" query))
                                :num-docs *search-page-number-results*
                                :first-doc start)))
    (values (iter (for doc in (montezuma::score-docs docs))
                  (let ((article (article-with-title
                                  (montezuma:document-value (montezuma:get-document *search-index*
                                                                                    (montezuma:doc doc))
                                                            "title"))))
                    (when article
                      (collect (cons article
                                     (montezuma:score doc))))))
            (montezuma::total-hits docs))))
                            

(restas:define-route search-page ("specials/search")
  (let ((query (hunchentoot:get-parameter "query"))
        (start (or (ignore-errors
                     (parse-integer
                      (hunchentoot:get-parameter "start")))
                   0)))
    (multiple-value-bind (articles total) (search-articles query start)
      (list :search-page
            :query (hunchentoot:get-parameter "query")
            :start start
            :articles articles
            :total total))))