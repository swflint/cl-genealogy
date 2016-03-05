;;;; genie.asd
;;;;
;;;; Copyright (c) 2015 Samuel Flint <swflint@lisp.technology>

(asdf:defsystem #:cl-genealogy
  :description "Describe genie here"
  :author "Samuel Flint <swflint@lisp.technology>"
  :license "GNU GPLv3 or Later"
  :depends-on (#:cl-utilities
               #:esrap
               #:parse-number
               #:lambdalite
               #:iterate)
  :serial t
  :components ((:file "package")
               (:file "database")
               (:file "family-tree")
               (:file "ahnentafel")))
