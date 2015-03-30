;;;; templates.lisp

(in-package #:genie)

;;; "templates" goes here. Hacks and glory await!

(defmacro define-html-template (name input &optional docstring)
  (let* ((old-name name)
         (name (intern (string-upcase old-name)))
         (docstring (if docstring
                        docstring
                        (format nil "Template caller for ~a, as ~a" old-name name)))
         (input (merge-pathnames input (asdf:system-source-directory '#:genie))))
    `(defun ,name (&rest args)
       ,docstring
       (with-output-to-string (*standard-output*)
         (funcall (create-template-printer ,input) args)))))


(defmacro generate-static (name filename type script)
  (declare (string filename)
           (symbol type)
           (type (or string symbol) name))
  (let* ((name (intern (string-upcase name)))
         (type (getf (list :js "application/javascript"
                           :javascript "application/javascript"
                           :css "text/css")
                     type)))
    `(define-route ,name (,filename
                          :content-type ,type)
       ,script)))


(defmacro define-css-page (name filename &rest body)
  (let* ((name (intern (string-upcase filename)))
         (css-chunks (map 'list #'compile-and-write body))
         (css (format nil "~{~a~^~%~%~}" css-chunks)))
    `(generate-static ,name
                      ,filename
                      :css
                      ,css)))

(defmacro define-js-page (name filename &rest script)
  (let ((name (intern (string-upcase filename))))
    `(generate-static ,name
                      ,filename
                      :javascript
                      (ps ,@script))))

(setq parenscript:*js-string-delimiter* #\")
(setq html-template:*string-modifier* #'cl:identity)

(define-css-page main.css "main.css"
  (*
   :margin 0
   :padding 0
   :font-family "Georgia, Palantino, Times, 'Times New Roman', sans-serif")
  (body
   :backgroud "#fff")
  (a
   :text-decoration "none")
  ("a:link, a:visited"
   :color "#f30")
  ("a:hover"
   :color "#f90")
  (div.main-content
   :position "absolute"
   :top "40px"
   :left "280px"
   :width "500px"
   (h1
    :font-size "40px"
    :font-weight "normal"
    :line-height "43px"
    :letter-spacing "-1px")
   (div.results
    :margin-top "20px"
    (ul
     :list-style-type "none")
    (li
     :font-size "18px"
     :line-height "24px"
     :margin-left "70px"))
   (div.page-list
    :text-align "center"
    :float "bottom")
   (dl
    :margin-top "10px")
   (dt
    :font-size "18px"
    :font-weight "bold")
   (dd
    :margin-left "20px"
    :margin-bottom "15px"))
  (div.nav
   :position absolute
   :top "40px"
   :left "20px"
   :width "200px"
   :padding "20px 20px 0 0"
   :border-right "1px solid #ccc"
   :text-align "right"
   (h2
    :text-transform "uppercase"
    :font-size "13px"
    :color "#333"
    :letter-spacing "1px"
    :line-height "20px"
    (a
     :color "#333"))
   (ul
    :list-style-type "none"
    :margin "20px 0")
   (li
    :font-size "14px"
    :line-height "20 px")
   (hr
    :height "1px"
    :color "#ccc"
    :margin-top "5px"
    :margin-bottom "5px")))

(define-html-template main-page "mainpage.httmpl")
(define-html-template person-page "view-person.httmpl")
