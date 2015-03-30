;;;; routes.lisp

(in-package #:genie)

;;; "routes" goes here. Hacks and glory await!

(defun generate-nav (main &optional sub)
  (declare (ignorable main sub))
  `((:heading "Add" :href ,(genurl 'add))
    (:title "Person" :href ,(genurl 'add/person))
    (:title "Death" :href ,(genurl 'add/death))
    (:title "Marriage" :href ,(genurl 'add/marriage))
    (:title "Divorce" :href ,(genurl 'add/divorce))
    (:heading "Notes" :href ,(genurl 'notes))
    (:title "Add" :href ,(genurl 'notes/add))
    (:title "View" :href ,(genurl 'notes/view))
    (:title "Edit" :href ,(genurl 'notes/edit))
    (:heading "Records" :href ,(genurl 'records))
    (:title "Add" :href ,(genurl 'records/add))
    (:title "View" :href ,(genurl 'records/view))
    (:heading "Reports" :href ,(genurl 'reports))
    (:title "Add" :href ,(genurl 'reports/add))
    (:title "View" :href ,(genurl 'reports/view))
    (:title "Edit" :href ,(genurl 'reports/edit))
    (:title "Generate" :href ,(genurl 'reports/generate))))

(define-route main ("")
  (main-page :nav (generate-nav :main)))

(define-route add ("add")
  (main-page :nav (generate-nav :main)
             :title "Add"
             :content
             (with-html-output-to-string (out)
               (:dl
                (:dt (:a :href (str (genurl 'add/person)) "Add Person"))
                (:dd "Add a person to the genealogical database.")
                (:dt (:a :href (str (genurl 'add/death)) "Add Death"))
                (:dd "Add a death record to the genealogical database.")
                (:dt (:a :href (str (genurl 'add/marriage)) "Add Marriage"))
                (:dd "Add a marriage record to the genealogical database.")
                (:dt (:a :href (str (genurl 'add/divorce)) "Add Divorce"))
                (:dd "Add a divorce record to the genealogical database")))))
(define-route add/person ("add/person"))
(define-route add/person/post ("add/person" :method :post))
(define-route add/marriage ("add/marriage"))
(define-route add/divorce ("add/divorce"))
(define-route add/death ("add/death"))

(define-route notes ("notes")
  (main-page :title "Notes Management"
             :nav (generate-nav :main)
             :content
             (with-html-output-to-string (output)
               (:dl
                (:dt (:a :href (str (genurl 'notes/add)) "Add Note"))
                (:dd "Add a research note.")
                (:dt (:a :href (str (genurl 'notes/view)) "View Notes"))
                (:dd "View research notes.")
                (:dt (:a :href (str (genurl 'notes/edit)) "Edit Note"))
                (:dd "Select and edit research notes.")))))
(define-route notes/add ("notes/add"))
(define-route notes/edit ("notes/edit"))
(define-route notes/view ("notes/view"))

(define-route records ("records")
  (main-page :title "Records Management"
             :nav (generate-nav :main)
             :content
             (with-html-output-to-string (output)
               (:dl
                (:dt (:a :href (str (genurl 'records/add)) "Add Record"))
                (:dd "Add a copy of a historical record/citation.")
                (:dt (:a :href (str (genurl 'records/view)) "View Records"))
                (:dd "View records/citations.")))))
(define-route records/add ("records/add"))
(define-route records/view ("records/view"))

(define-route reports ("reports")
  (main-page :title "Reports"
             :nav (generate-nav :main)
             :content
             (with-html-output-to-string (output)
               (:dl
                (:dt (:a :href (str (genurl 'reports/add)) "Add Report"))
                (:dd "Add a report of one of the following types:"
                     (:ul
                      (:li "Family Tree")
                      (:li "Ahnentafel")
                      (:li "Biography")
                      (:li "Complete genealogical report")))
                (:dt (:a :href (str (genurl 'reports/view)) "View Report"))
                (:dd "View report properties/report.")
                (:dt (:a :href (str (genurl 'reports/edit)) "Edit Report"))
                (:dd "Edit report properties/report content.")
                (:dt (:a :href (str (genurl 'reports/generate)) "Generate Report"))
                (:dd "Generate formatted report output.")))))
(define-route reports/add ("reports/add"))
(define-route reports/view ("reports/view"))
(define-route reports/edit ("reports/edit"))
(define-route reports/generate ("reports/generate"))

(define-route search-page ("search"))
(define-route search/advanced ("search/advanced"))
