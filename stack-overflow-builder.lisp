(defpackage #:stack-overflow-builder
  (:use #:cl)
  (:local-nicknames (#:pg #:postmodern)
                    (#:%pg #:cl-postgres)
                    (#:pg-error #:cl-postgres-error)
                    (#:bt #:bordeaux-threads-2)
                    (#:ll #:lparallel)
                    (#:ll.q #:lparallel.queue)))
(in-package #:stack-overflow-builder)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf cl-postgres:*sql-readtable*
        (cl-postgres:copy-sql-readtable simple-date-cl-postgres-glue:*simple-date-sql-readtable*)
        ll:*kernel*
        (ll:make-kernel 16 :name "stack-overflow-builder")))

(defvar *numeric-columns*
  '("id" "user-id" "class" "reputation" "views" "upvotes" "downvotes" "account-id"
    "post-notice-duration-id" "classid" "score" "view-count" "comment-count"
    "favorite-count" "last-editor-user-id" "accepted-answer-id" "parent-id"
    "owner-user-id" "answer-count" "post-id" "post-notice-type-id" "deletion-user-id"
    "related-post-id" "link-type-id" "post-type-id" "creation-moderator-id"
    "approval-moderator-id" "deactivation-moderator-id" "flag-type-id"
    "close-reason-type-id" "close-as-offtopic-reason-type-id"
    "duplicate-of-question-id" "vote-type-id" "bounty-amount" "suggested-edit-id"
    "target-rep-change" "target-user-id" "excerpt-post-id" "wiki-post-id" "tag-id"
    "auto-rename-count" "approved-by-user-id" "post-history-type-id"))

(defun camel->kebab (column-name)
  "Convert a CamelCase name to kebab-case."
  (let ((kebab (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t)))
    (vector-push-extend (elt column-name 0) kebab)
    (loop :for char across (subseq column-name 1)
          :do (when (char= char (char-upcase char))
                (vector-push-extend #\- kebab))
              (vector-push-extend char kebab)
          :finally (return (string-downcase kebab)))))

(defun normalize-names (row)
  "Ensure columns are transformed from CamelCase to kebab-case (that is
automatically converted to snake_case by Postmodern during a
transaction/query)."
  (loop :for (name value) in row
        :collecting (list (camel->kebab name) value)))

(defun clean-row (row)
  "Return a flattened list of columns and their values. Converts strings to
numbers when needed."
  (flet ((numeric-column (column value)
           (when (member column *numeric-columns* :test #'string=)
             (parse-integer value))))
    (loop :for (name value) in (normalize-names row)
          :for n = (numeric-column name value)
          :collecting (list `',(read-from-string name) (or n value)))))

(defun row-values (row)
  (values (mapcar #'first row)
          (mapcar #'second row)))

(defun build-query (table columns values)
  `(pg:execute (:insert-rows-into ',table
                :columns ,@columns
                :values '(,values)
                :on-conflict 'id :do-nothing)))

;; For testing
(defun print-rows (source)
  (with-open-file (s (pathname source) :direction :input)
    (loop :for row = (read-line s nil) :while row
          :when row :collect it)))

(defun 5min ()
  (local-time:timestamp+ (local-time:now) 5 :minute))

(defun skip-count (table)
  (let ((count (caar (pg:query (format nil "select count (*) from ~s" table)))))
    (if (zerop count) 0
        ;; Account for the first two lines of the file which aren't valid rows
        (+ 2 count))))

(defun insert-rows (rows table)
  (unless (null rows)
    (pg:with-transaction ()
      (loop :for row across rows
            :when row
            :do (multiple-value-bind (columns values) (row-values row)
                  (eval (build-query table columns values)))))))

(defun import-data (filename table &optional (count 50000))
  (let ((buf (make-array count))
        (log-interval (5min))
        (rows-processed 0))
    (with-open-file (s (pathname filename) :direction :input)
      (flet ((clear-buffer ()
               (dotimes (i count)
                 (setf (aref buf i) nil)))
             (fill-buffer  ()
               (dotimes (i count t)
                 (let ((row (read-line s nil)))
                   (if (null row) (return)
                       (setf (aref buf i) row)))))
             (process-buffer ()
               (ll:pmap 'vector (lambda (row)
                                  (let ((parsable (xmls:parse row)))
                                    (when parsable (clean-row (xmls:node-attrs parsable)))))
                        :size count
                        :parts 16
                        buf)))
        (log:info "Seeing if any rows are already imported, this could take a while...~%~%")
        (let ((skippable-rows (skip-count table)))
          (loop :until (zerop skippable-rows)
                :do (read-line s nil)
                    (incf rows-processed)
                    (decf skippable-rows)))
        (log:info "Starting import now @ row ~:d - fingers crossed.~%~%" rows-processed)
        (loop :while (fill-buffer)
              :do (insert-rows (process-buffer) table)
                  (clear-buffer)
                  (incf rows-processed count)
                  (when (local-time:timestamp>= (local-time:now) log-interval)
                    (log:info "Rows processed: ~:d" rows-processed)
                    (setf log-interval (5min)))
              :finally (insert-rows (process-buffer) table))))))
