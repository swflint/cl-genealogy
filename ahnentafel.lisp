;;;; ahnentafel.lisp

(in-package #:cl-genealogy)

;;; "ahnentafel" goes here. Hacks and glory await!

(defun generate-ahnentafel-numbers (starting-person number-of-generations)
  (flet ((get-father (person)
           (:/father (get-birth person)))
         (get-mother (person)
           (:/mother (get-birth person)))
         (generate-number (current gender)
           (if (string= gender "M")
               (* 2 current)
               (1+ (* 2 current)))))
    (let ((ahnentafel-list (cons (cons 1 starting-person) nil)))
      (labels ((recurse (person number generation gender)
                 (if (not (= generation 0))
                     (let ((new-number (generate-number number gender))
                           (father (get-father person))
                           (mother (get-mother person)))
                       (push (cons new-number person) ahnentafel-list)
                       (if (not (= 0 father))
                           (recurse father new-number (1- generation) "M"))
                       (if (not (= 0 mother))
                           (recurse mother new-number (1- generation) "F"))))))
        (recurse (get-father starting-person) 1 (1- number-of-generations) "M")
        (recurse (get-mother starting-person) 1 (1- number-of-generations) "F"))
      ahnentafel-list)))

(defun generate-ahnentafel-text (ahnentafel)
  (map 'list (lambda (record)
               (let* ((num (car record))
                      (person (cdr record))
                      (name (:/person-name (get-person person)))
                      (birthdate (:/birth-date (get-birth person)))
                      (death (let ((death-record (get-death person)))
                               (if (null death-record)
                                   ""
                                   (format nil " -- ~a" (:/death-date death-record))))))
                 (cons num
                       (format nil
                               "~a, ~a~a"
                               name
                               birthdate
                               death))))
       ahnentafel))

(defun print-ahnentafel (file start generations)
  (let* ((ahnentafel-numbers (generate-ahnentafel-numbers start generations))
         (ahnentafel-with-text (generate-ahnentafel-text ahnentafel-numbers))
         (sorted-ahnentafel (sort ahnentafel-with-text #'< :key #'car))
         (sorted-ahnentafel-mapping (sort ahnentafel-numbers #'< :key #'car)))
    (with-open-file (output file
                            :direction :output
                            :if-exists :overwrite
                            :if-does-not-exist :create)
      (iter (for (number . text) in sorted-ahnentafel)
            (format output "~10,5R: ~A~&" number text)))
    (values sorted-ahnentafel-mapping sorted-ahnentafel)))
