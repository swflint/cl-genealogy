;;;; genie.asd
;;;;
;;;; Copyright (c) 2015 Samuel Flint <swflint@lisp.technology>

(asdf:defsystem #:genie
  :description "Describe genie here"
  :author "Samuel Flint <swflint@lisp.technology>"
  :license "GNU GPLv3 or Later"
  :depends-on (#:restas
               #:3bmd
               #:babel
               #:cl-utilities
               #:cl-who
               #:esrap
               #:html-template
               #:ironclad
               #:lass
               #:parenscript
               #:parse-number
               #:lambdalite
               #:iterate
               #:archive)
  :serial t
  :components ((:file "package")
               (:file "database")
               (:file "family-tree")
               (:file "genie")))
