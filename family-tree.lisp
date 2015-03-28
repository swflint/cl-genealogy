;;;; family-tree.lisp

(in-package #:genie)

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
;; (defun generate-person-edges (person-id)
;;   (let ((birth (first (select :births (where (equal :/person person-id))))))
;;     (if (or (= 0 (:/father birth))
;;            (= 0 (:/mother birth)))
;;         (if (= 0 (:/mother birth))
;;             (format nil "person_~a -> person_~a;" (:/father birth) person-id)
;;             (if (= 0 (:/father birth))
;;                 (format nil "person_~a -> person_~a;" (:/mother birth) person-id)
;;                 ""))
;;         (let ((marriage
;;                (first (select :marriages
;;                               (where (and (equal :/wife (:/mother birth))
;;                                         (equal :/husband (:/father birth))))))))
;;           (if (not (null marriage))
;;               (format nil "marriage_~a -> person_~a;"
;;                       (:/marriage-id marriage)
;;                       person-id)
;;               (format nil "{person_~a, person_~a} -> person_~a"
;;                       (:/mother birth)
;;                       (:/father birth)
;;                       person-id))))))

(defun generate-person-edges (person-id)
  (let* ((birth
          (first (select :births (where (equal :/person person-id)))))
         (mother (:/mother birth))
         (father (:/father birth))
         (marriage
          (first (select :marriages
                         (where (and (equal :/wife mother)
                                   (equal :/husband father)))))))
    (cond
      ((= 0 (:/father birth))
       )
      ((= 0 (:/mother birth))
       (format nil "person_~a -> person_~a;" father person-id))
      ((null marriage)
       (format nil "{person_~a, person_~a} -> person_~a;"
               mother
               father
               person-id))
      (t
       (format nil "marriage_~a -> person_~a;"
               (:/marriage-id marriage)
               person-id)))))

(defun generate-person-edges (person-id)
  (flet ((unlisted-father (birth-record)
           (= 0 (:/father birth-record)))
         (unlisted-mother (birth-record)
           (= 0 (:/mother birth-record)))
         (parent-married-status (birth-record)
           (let* ((mother (:/mother birth-record))
                  (father (:/father birth-record))
                  (records (select :marriages))
                  (record (first
                           (remove-if (lambda (record)
                                        (not (= (:/wife record) mother)))
                                      (remove-if (lambda (record)
                                                   (not (= (:/husband record) father)))
                                                 records)))))
             (values (not (null record)) (:/marriage-id record)))))
    (let ((birth (first (select :births
                                (where (equal :/birth-person person-id))))))
      (cond
        ((unlisted-father birth)
         (format nil "person_~a -> person_~a;" (:/mother birth) person-id))
        ((unlisted-mother birth)
         (format nil "person_~a -> person_~a;" (:/father birth) person-id))
        ((parent-married-status birth)
         (multiple-value-bind (bool id)
             (parent-married-status birth)
           (declare (ignore bool))
           (format nil "marriage_~a -> person_~a;"
                   id
                   person-id)))
        ((not (parent-married-status birth))
         (format nil "{person_~a, person_~a} -> person_~a;"
                 (:/mother birth)
                 (:/father birth)
                 person-id))))))
;;; Generate nodes for weddings
(defun generate-marriage-node (marriage-id)
  (let ((marriage (first (select :marriages (where (equal :/marriage-id
                                                          marriage-id)))))
        (divorce (first (select :divorces (where (equal :/marriage marriage-id))))))
    (format nil "marriage_~a [label = \"Married ~a &mdash; ~a\", shape = invtrapeseum];"
            marriage-id
            (:/wedding-date marriage)
            (if (not (null divorce))
                (if (string= "0" (:/end-date marriage))
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

;; (defun generate-graph ()
;;   ())
