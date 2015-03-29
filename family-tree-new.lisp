;;;; family-tree-new.lisp

(in-package #:genie)

;;; "family-tree-new" goes here. Hacks and glory await!

;;; Generate Edges between people
(defun generate-people-edges (person)
  (let* ((birth (get-birth person))
         (mother (:/mother birth))
         (father (:/father birth)))
    (cond
      ((and (= 0 father)
          (= 0 mother))
       nil)
      ((= 0 father)
       (list :mother-child mother person))
      ((= 0 mother)
       (list :father-child father person))
      (t
       (let ((marriage (get-marriage father mother)))
         (if (null marriage)
             (list :both-to-child father mother person)
             (list :marriage-child (:/marriage-id marriage) person)))))))

;;; Generate edges for a marriage
(defun generate-wedding-edges (marriage)
  (let* ((marriage (get-marriage-id marriage))
         (husband (:/husband marriage))
         (wife (:/wife marriage)))
    (list :marriage marriage husband wife)))

;;; Generate Node statement
(defun generate-node-statement (type id)
  (case type
    (:marriage
     (let* ((marriage (get-marriage-id id))
            (start (:/wedding-date marriage))
            (end (:/end-date marriage))
            (divorce (get-divorce id)))
       (cond
         ((and (null divorce)
             (string= "0" end))
          (format nil
                  "marriage_~a [label = \"Married ~a &mdash; Current\", shape = invtrapezium];"
                  id
                  start))
         ((null divorce)
          (format nil
                  "marriage_~a [label = \"Married ~a &mdash; ~a\", shape = invtrapezium];"
                  id
                  start
                  end))
         (t
          (format nil
                  "marriage_~a [label = \"Married ~a &mdash; Div. ~a\", shape = invtrapezium];"
                  id
                  start
                  end)))))
    (:person
     (let* ((person (get-person id))
            (name (:/person-name person))
            (color (if (string= (:/gender person) "M") "blue" "pink"))
            (birth (get-birth id))
            (date (:/birth-date birth))
            (death (get-death id)))
       (if (null death)
           (format nil
                   "person_~a [label \"~a\\n~a &mdash; Living\", shape = rectangle, color = ~a];"
                   id
                   name
                   date
                   color)
           (format nil
                   "person_~a [label \"~a\\n~a &mdash; ~a\", shape = rectangle, color = ~a];"
                   id
                   name
                   date
                   (:/death-date death)
                   color))))))

;;; generate correct edge statement
(defun generate-edge-statement (edge)
  (let ((type (first edge)))
    (case type
      (:mother-child
       (format nil "person_~a -> person_~a;"
               (second edge)
               (third edge)))
      (:father-child
       (format nil "person_~a -> person_~a;"
               (second edge)
               (third edge)))
      (:both-to-child
       (format nil "{person_~a, person_~a} -> person_~a;"
               (second edge)
               (third edge)
               (fourth edge)))
      (:marriage-child
       (format nil "marriage_~a -> person_~a;"
               (second edge)
               (third edge)))
      (:marriage
       (format nil "{person_~a, person_~a} -> marriage_~a;"
               (third edge)
               (fourth edge)
               (second edge)))
      (t
       nil))))

;;; Generate edge list
(defun generate-complete-edge-list ()
  (let ((people-edges (iter (for i from 1 to (length (select :people)))
                            (collect (generate-people-edges i))))
        (weddings (iter (for i from 1 to (length (select :marriages)))
                        (collect (generate-marriage-edges i)))))
    (list weddings people-edges)))

;;; generate dot file
(defun generate-dot-file (file-name)
  (let* ((edge-list (generate-complete-edge-list))
         (weddings (first edge-list))
         (people (second edge-list))
         (wedding-nodes (iter (for i from 1 to (length (select :marriags)))
                              (collect (generate-node-statement :marriage i))))
         (people-nodes (iter (for i from 1 to (length (select :people)))
                             (collect (generate-node-statement :person i))))
         (people-edges (map 'list #'generate-edge-statement people))
         (wedding-edges (map 'list #'generate-edge-statement weddings)))
    (with-open-file (output file-name
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (format output
              "digraph {
~@{	~a~^~&~}

~@{	~a~^~&~}

~@{	~a~^~&~}

~@{	~a~^~&~}
}"
              people-nodes
              wedding-nodes
              wedding-edges
              people-edges))))
