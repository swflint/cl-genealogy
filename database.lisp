;;;; database.lisp

(in-package #:cl-genealogy)

;;; "database" goes here. Hacks and glory await!
(defmacro constrain-values (&rest values)
  `(lambda (object)
     (the boolean
          (member object (list ,@values)))))

(defmacro in-table-column (table column)
  `(lambda (object)
     (let ((possible-values (iter
                              (for row in (select ,table))
                              (collect (,column row)))))
       (the boolean (member object possible-values)))))

(defmacro ids-in-table-column (table column)
  `(lambda (object)
     (let ((possible-values (iter
                              (for row in (select ,table))
                              (collect (,column row)))))
       (and (listp object)
          (reduce #'and (map 'list ,(in-table-column table column)
                           object))))))

(defmacro unique-in-column (table column type)
  `(lambda (object)
     (if (typep object ',type)
         (let ((possible-values (iter
                                  (for row in (select ,table))
                                  (collect (,column row)))))
           (not (the boolean (member object possible-values)))))))

(defun generate-table-id (table)
  (declare (keyword table))
  (1+ (length (select table))))

;;; The Person table
(defattributes
  :/person-id (unique-in-column :people :/person-id integer)
  :/person-name #'stringp
  :/gender (constrain-values "M" "F"))

;;; The Births table
(defattributes
  :/birth-id (unique-in-column :births :/birth-id integer)
  :/birth-person (in-table-column :people :/person-id)
  :/birth-date #'stringp
  :/father (lambda (object)
             (or (= 0 object)
                (funcall (in-table-column :people :/person-id) object)))
  :/mother (lambda (object)
             (or (= 0 object)
                (funcall (in-table-column :people :/person-id) object))))

;;; The Deaths table
(defattributes
  :/death-id (unique-in-column :deaths :/death-id integer)
  :/death-person (in-table-column :people :/person-id)
  :/death-date #'stringp)


;;; The Marriages table
(defattributes
  :/marriage-id (unique-in-column :marriages :/marriage-id integer)
  :/husband (in-table-column :people :/person-id)
  :/wife (in-table-column :people :/person-id)
  :/wedding-date #'stringp
  :/end-date #'stringp)


;;; The Divorces table
(defattributes
  :/divorce-id (unique-in-column :divorces :/divorce-id integer)
  :/marriage (in-table-column :marriages :/marriage-id)
  :/divorce-date #'stringp)

;;; Common to the notes/records
(defattributes
  :/person (in-table-column :people :/person-id)
  :/birth (in-table-column :births :/birth-id)
  :/death (in-table-column :deaths :/death-id)
  :/marriage (in-table-column :marriages :/marriage-id)
  :/divorce (in-table-column :divorces :/divorce-id))

;;; The Notes table
(defattributes
  :/note-id (unique-in-column :notes :/note-id integer)
  :/note-title #'stringp
  :/note-text #'stringp
  :/media-link #'stringp)

;;; The Reports Table
(defattributes
  :/report-id (unique-in-column :reports :/report-id integer)
  :/report-title #'stringp
  :/report-type (constrain-values "tree" "full" "ahnentafel"))

;;; insert person
(defun new-person (name gender birth-date mother father)
  (declare (string name)
           (integer mother
                    father))
  (with-tx
    (let ((person-id (generate-table-id :people))
          (birth-id (generate-table-id :births)))
      (insert :people
              (list :/person-id person-id
                    :/person-name name
                    :/gender gender))
      (insert :births
              (list :/birth-id birth-id
                    :/birth-person person-id
                    :/birth-date birth-date
                    :/mother mother
                    :/father father))
      (values person-id birth-id))))

;;; insert death
(defun new-death (person death-date)
  (with-tx
    (let ((death-id (generate-table-id :deaths)))
      (insert :deaths
              (list :/death-id death-id
                    :/death-person person
                    :/death-date death-date))
      death-id)))

;;; insert marriage
(defun new-marriage (husband wife date end-date)
  (with-tx
    (let ((marriage-id (generate-table-id :marriages)))
      (insert :marriages
              (list :/marriage-id marriage-id
                    :/husband husband
                    :/wife wife
                    :/wedding-date date
                    :/end-date date))
      marriage-id)))

;;; insert divorce
(defun new-divorce (marriage-id date)
  (with-tx
    (let ((divorce-id (generate-table-id :divorces)))
      (insert :divorces
              (list :/divorce-id divorce-id
                    :/marriage marriage-id
                    :/divorce-date date))
      divorce-id)))


;;; Query records
;;; Get Person
(defun get-person (id)
  (with-tx
    (first
     (select :people
             (where (equal :/person-id id))))))

;;; Get Birth record (by person)
(defun get-birth (id)
  (with-tx
    (first
     (select :births
             (where (equal :/birth-person id))))))

;;; Get Death Record
(defun get-death (id)
  (with-tx
    (first
     (select :deaths
             (where (equal :/death-person id))))))

;;; get marriage (by husband/wife)
(defun get-marriage (husband wife)
  (with-tx
    (first
     (select :marriages
             (where (and (equal :/wife wife)
                       (equal :/husband husband)))))))

;; get marriage (by id)
(defun get-marriage-id (marriage-id)
  (with-tx
    (first
     (select :marriages
             (where (and (equal :/marriage-id marriage-id)))))))

;;; get divorce (by marriage)
(defun get-divorce (id)
  (with-tx
    (first
     (select :divorces
             (where (equal :/marriage id))))))
