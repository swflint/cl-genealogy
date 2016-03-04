;;;; family-tree.lisp

(in-package #:cl-genealogy)

;;; "family-tree" goes here. Hacks and glory await!

;;; Generate nodes for people
(defun generate-person-node (person-id)
  (let ((person (get-person person-id))
        (birth (get-birth person-id))
        (death (get-death person-id)))
    (format nil "person_~a [label = \"~a\", color = ~a, shape = rectangle];"
            person-id
            (format nil "~a\\\n~a &mdash; ~a"
                    (:/person-name person)
                    (:/birth-date birth)
                    (if (not (null death))
                        (:/death-date death)
                        "Living"))
            (if (string= (:/gender person) "M") "blue" "pink"))))

;;; Generate edges between people
(defun generate-person-edges (person-id)
  (let* ((birth (get-birth person-id))
         (mother (:/mother birth))
         (father (:/father birth)))
    (cond
      ((and (= 0 father)
          (= 0 mother))
       "")
      ((= 0 father)
       (format nil "person_~a -> person_~a;" mother person-id))
      ((= 0 mother)
       (format nil "person_~a -> person_~a;" father person-id))
      (t
       (let ((marriage (get-marriage father mother)))
         (if (null marriage)
             (format nil "{person_~a, person_~a} -> person_~a;"
                     mother
                     father
                     person-id)
             (format nil "marriage_~a -> person_~a;"
                     (:/marriage-id marriage)
                     person-id)))))))

;;; Generate nodes for weddings
;; (defun generate-marriage-node (marriage-id)
;;   (let ((marriage (first (select :marriages (where (equal :/marriage-id
;;                                                           marriage-id)))))
;;         (divorce (first (select :divorces (where (equal :/marriage marriage-id))))))
;;     (format nil "marriage_~a [label = \"Married ~a &mdash; ~a\", shape = invtrapezium];"
;;             marriage-id
;;             (:/wedding-date marriage)
;;             (if (not (null divorce))
;;                 (if (string= "0" (:/end-date marriage))
;;                     "Present"
;;                     (:/end-date marriage))
;;                 (format nil "Div. ~a" (:/divorce-date divorce))))))

(defun generate-marriage-node (marriage-id)
  (let* ((marriage (get-marriage-id marriage-id))
         (start-date (:/wedding-date marriage))
         (end-date (:/end-date marriage))
         (divorce (get-divorce marriage-id)))
    (format nil "marriage_~a [label = \"Married ~a &mdash; ~a\", shape = none];"
            marriage-id
            start-date
            (if (null divorce)
                (if (string= "0" end-date)
                    "Present"
                    (:/end-date marriage))
                (format nil "Div. ~a" (:/divorce-date divorce))))))

;;; Generate edges for weddings
(defun generate-marriage-edges (marriage-id)
  (let* ((marriage (first (select :marriages
                                  (where (equal :/marriage-id marriage-id)))))
         (husband-id (:/husband marriage))
         (wife-id (:/wife marriage)))
    (format nil "{person_~a, person_~a} -> marriage_~a;"
            husband-id
            wife-id
            marriage-id)))

;;; Generate Family Tree
(defun generate-graph (file-name)
  (let ((people (iter (for i from 1 to (length (select :people)))
                      (collect (generate-person-node i))))
        (birth-edges (remove-if (lambda (edge)
                                  (string= edge ""))
                                (iter (for i from 1 to (length (select :people)))
                                      (collect (generate-person-edges i)))))
        (weddings (iter (for i from 1 to (length (select :marriages)))
                        (collect (generate-marriage-node i))))
        (wedding-edges (iter (for i from 1 to (length (select :marriages)))
                             (collect (generate-marriage-edges i)))))
    (with-open-file (out file-name
                         :direction :output
                         :if-exists :overwrite
                         :if-does-not-exist :create)
      (format out
              "digraph {
~{	~a~%~}
~{	~a~%~}
~{	~a~%~}
~{	~a~%~}
}"
              people
              weddings
              wedding-edges
              birth-edges))))
