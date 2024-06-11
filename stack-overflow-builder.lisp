(defpackage #:stack-overflow-builder
  (:use #:cl)
  (:local-nicknames (#:pg #:postmodern)
                    (#:%pg #:cl-postgres)
                    (#:pg-error #:cl-postgres-error)))
(in-package #:stack-overflow-builder)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf cl-postgres:*sql-readtable*
        (cl-postgres:copy-sql-readtable
         simple-date-cl-postgres-glue:*simple-date-sql-readtable*)))


(defclass posts ()
  ((id                       :col-type integer   :accessor id)
   (tags                     :col-type text      :accessor tags)
   (body                     :col-type text      :accessor body)
   (title                    :col-type text      :accessor title)
   (score                    :col-type integer   :accessor score)
   (parent-id                :col-type integer   :accessor parent-id)
   (view-count               :col-type integer   :accessor view-count)
   (closed-date              :col-type timestamp :accessor closed-date)
   (post-type-id             :col-type smallint  :accessor post-type-id)
   (answer-count             :col-type integer   :accessor answer-count)
   (creation-date            :col-type timestamp :accessor creation-date)
   (comment-count            :col-type integer   :accessor comment-count)
   (owner-user-id            :col-type integer   :accessor owner-user-id)
   (deletion-date            :col-type timestamp :accessor deletion-date)
   (favorite-count           :col-type integer   :accessor favorite-count)
   (last-edit-date           :col-type timestamp :accessor last-edit-date)
   (content-license          :col-type text      :accessor content-license)
   (accepted-answer-id       :col-type integer   :accessor accepted-answer-id)
   (owner-display-name       :col-type text      :accessor owner-display-name)
   (last-activity-date       :col-type timestamp :accessor last-activity-date)
   (last-editor-user-id      :col-type integer   :accessor last-editor-user-id)
   (community-owned-date     :col-type timestamp :accessor community-owned-date)
   (last-editor-display-name :col-type text      :accessor last-editor-display-name))
  (:metaclass pg:dao-class)
  (:table-name posts))


(defvar *numeric-columns*
  '("id" "user_id" "class" "reputation" "views" "upvotes" "downvotes" "account_id"
    "post_notice_duration_id" "classid" "score" "view_count" "comment_count"
    "favorite_count" "last_editor_user_id" "accepted_answer_id" "parent_id"
    "owner_user_id" "answer_count" "post_id" "post_notice_type_id" "deletion_user_id"
    "related_post_id" "link_type_id" "post_type_id" "creation_moderator_id"
    "approval_moderator_id" "deactivation_moderator_id" "flag_type_id"
    "close_reason_type_id" "close_as_offtopic_reason_type_id"
    "duplicate_of_question_id" "vote_type_id" "bounty_amount" "suggested_edit_id"
    "target_rep_change" "target_user_id" "excerpt_post_id" "wiki_post_id" "tag_id"
    "auto_rename_count" "approved_by_user_id" "post_history_type_id"))

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
               (vector-push-extend #\_ kebab))
             (vector-push-extend char kebab)
          finally (return (string-downcase kebab)))))

(defun normalize-names (row)
  "Ensure columns are transformed from CamelCase to kebab-case (that is automatically converted to snake_case)."
  (loop for (name value) in row
        collecting (list (camel->kebab name) value)))

(defun clean-row (row)
  "Return a flattened list of columns and their values. Converts strings to numbers when needed."
  (flet ((numeric-column (column value)
           (when (member column *numeric-columns* :test #'string=)
             (parse-integer value))))
    (loop for (name value) in (normalize-names row)
          for n = (numeric-column name value)
          collecting (list name (or n value)))))

(defun row-values (row) (values (mapcar #'first row) (mapcar #'second row)))

(defun build-query (table columns values)
  `(pg:query (:insert-rows-into ',table
              :columns ,@columns
              :values ',values
              :on-conflict 'id :do-nothing)))

(defun bulk-copy (table columns rows)
  (let ((writer (%pg:open-db-writer pomo:*database* table columns)))
    (unwind-protect (loop for row in rows
                          do (%pg:db-write-row writer row))
      (%pg:close-db-writer writer))))

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
                 (bulk-copy table key rows)
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
                   do (maybe-insert-rows table hash-table :count count)
                      (fxml.klacks:find-element data "row")
                      (when (local-time:timestamp>= (local-time:now) log-interval)
                        (log:info "Rows processed: ~d~%" rows-processed)
                        (setf log-interval (local-time:timestamp+ (local-time:now) 5 :minute)))
                   until (end-of-document-p data)
                   finally (maybe-insert-rows table hash-table :now T :count count))))
      (if table (import-table) (print-rows)))))
