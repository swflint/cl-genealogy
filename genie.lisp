;;;; genie.lisp
;;;;
;;;; Copyright (c) 2015 Samuel Flint <swflint@lisp.technology>

(restas:define-module #:genie
  (:use #:cl
        #:lambdalite
        #:config-parser
        #:restas
        #:cl-who
        #:iterate)
  (:import-from #:ironclad
                #:pbkdf2-hash-password
                #:byte-array-to-hex-string
                #:hex-string-to-byte-array)
  (:import-from #:hunchentoot
                #:post-parameter
                #:start-session
                #:session-value)
  (:import-from #:html-template
                #:create-template-printer)
  (:import-from #:lass
                #:compile-and-write))

(in-package #:genie)

;;; "genie" goes here. Hacks and glory await!
