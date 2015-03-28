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

(define-route add ("add"))
(define-route add/person ("add/person"))
(define-route add/marriage ("add/marriage"))
(define-route add/divorce ("add/divorce"))
(define-route add/death ("add/death"))

(define-route notes ("notes"))
(define-route notes/add ("notes/add"))
(define-route notes/edit ("notes/edit"))
(define-route notes/view ("notes/view"))

(define-route records ("records"))
(define-route records/add ("records/add"))
(define-route records/view ("records/view"))

(define-route reports ("reports"))
(define-route reports/add ("reports/add"))
(define-route reports/view ("reports/view"))
(define-route reports/edit ("reports/edit"))
(define-route reports/generate ("reports/generate"))

(define-route search-page ("search"))
(define-route search/advanced ("search/advanced"))
