(defpackage #:stack-overflow-builder
  (:use #:cl)
  (:local-nicknames (#:pg-error #:cl-postgres-error)
                    (#:pg #:pomo)))
(in-package #:stack-overflow-builder)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf cl-postgres:*sql-readtable*
        (cl-postgres:copy-sql-readtable
         simple-date-cl-postgres-glue:*simple-date-sql-readtable*)))

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

(defun end-of-document-p (source)
  (eq (fxml.klacks:peek source) :end-document))

(defun grab-row (source handler)
  "Return a parsed and usable row when possible."
  (multiple-value-bind (type namespace local-name qualified-name)
      (fxml.klacks:peek source)
    (declare (ignore namespace local-name))
    (when (and (eq type :start-element)
               (string= qualified-name "row"))
      (first (rest (fxml.klacks:serialize-element source handler))))))

(defun camel->kebab (column-name)
  "Convert a CamelCase name to kebab-case."
  (let ((kebab (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t)))
    (vector-push-extend (elt column-name 0) kebab)
    (loop for char across (subseq column-name 1)
          do (when (char= char (char-upcase char))
               (vector-push-extend #\- kebab))
             (vector-push-extend char kebab)
          finally (return (string-downcase kebab)))))

(defun normalize-names (row)
  "Ensure columns are transformed from CamelCase to kebab-case (that is automatically converted to snake_case)."
  (flet ((normalize (column)
           (let* ((name (first column))
                  (value (second column))
                  (snake-case-name (camel->kebab name)))
             (list (or snake-case-name name) value))))
    (loop for column in row while row
          collecting (normalize column))))

(defun clean-row (row)
  "Return a flattened list of columns and their values. Converts strings to numbers when needed."
  (flet ((numeric-column (column value)
           (when (member (string-downcase (first column)) *numeric-columns* :test #'string=)
             (parse-integer value))))
    (mapcar (lambda (column)
              (let* ((name (nth-value 0 (read-from-string (first column))))
                     (value (second column))
                     (n (numeric-column column value)))
                (list `',name (or n value))))
            (normalize-names row))))

(defun row-values (row) (values (mapcar #'first row) (mapcar #'second row)))

(defun build-query (table columns values)
  `(pg:query (:insert-rows-into ',table
              :columns ,@columns
              :values ',values
              :on-conflict 'id :do-nothing)))

(defun push-key-value (columns values hash-table)
  "Set key COLUMNS with value VALUES in HASH-TABLE. Creates key COLUMNS if needed."
  (if (gethash columns hash-table)
      (push values (gethash columns hash-table))
      (setf (gethash columns hash-table)
            (list values))))

(defun maybe-insert-rows (table hash-table &key now (count 1000))
  (loop for key being the hash-keys of hash-table
        do (let ((rows (gethash key hash-table)))
             (unless (null rows)
               (when (or now (= (length rows) count))
                 (eval (build-query table key rows))
                 (setf (gethash key hash-table) nil))))))

(defun import-data (source &optional table (count 1000))
  "Import data from SOURCE pathname. Optionally place data into TABLE using
pomo:*database* as the database, otherwise returns data to standard output."
  (let ((log-interval (local-time:timestamp+ (local-time:now) 5 :minute))
        (data (fxml:make-source (pathname source)))
        (handler (fxml.xmls:make-xmls-builder))
        (hash-table (make-hash-table :test #'equal)))
    (flet ((print-rows ()
             (loop for row = (grab-row data handler)
                   when row collect it
                   until (end-of-document-p data)
                   do (fxml.klacks:peek-next data)))
           (import-table ()
             (log:info "Starting import now.~%")
             (loop for row = (grab-row data handler)
                   when row do (multiple-value-bind (columns values) (row-values (clean-row row))
                                 (push-key-value columns values hash-table))
                   and count row into rows-processed
                   until (end-of-document-p data)
                   do (maybe-insert-rows table hash-table :count count)
                      (fxml.klacks:peek-next data)
                      (when (local-time:timestamp>= (local-time:now) log-interval)
                        (log:info "Rows processed: ~d~%" rows-processed)
                        (setf log-interval (local-time:timestamp+ (local-time:now) 5 :minute)))
                   finally (maybe-insert-rows table hash-table :now T :count count))))
      (if table (import-table) (print-rows)))))
