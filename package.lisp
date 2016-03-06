;;;; package.lisp
;;;;
;;;; Copyright (c) 2015 Samuel Flint <swflint@lisp.technology>

(defpackage #:config-parser
  (:use :esrap
        :cl)
  (:import-from #:parse-number
                #:parse-number)
  (:export open-configuration-file))

(defpackage #:cl-genealogy
  (:use #:cl
        #:lambdalite
        #:iterate
        #:archive)
  (:export generate-graph
           print-ahnentafel
           database
           new-person
           new-death
           new-marriage
           new-divorce))
