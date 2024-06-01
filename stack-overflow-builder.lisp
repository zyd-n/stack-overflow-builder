(defpackage #:stack-overflow-builder
  (:use #:cl)
  (:local-nicknames
   (#:pg-error #:cl-postgres-error)
   (#:pg #:pomo)))
(in-package #:stack-overflow-builder)

;; TODO: We don't have to do this when we create tables from Lisp rather than
;; directly with SQL.
(defvar *camel->snake*
  '(("TagId" "tag-id")
    ("UserId" "user-id")
    ("PostId" "post-id")
    ("AboutMe" "about-me")
    ("TagName" "tag-name")
    ("TagBased" "tag-based")
    ("IsHidden" "is-hidden")
    ("ParentId" "parent-id")
    ("AccountId" "account-id")
    ("ViewCount" "view-count")
    ("WebsiteUrl" "website-url")
    ("ClosedDate" "closed-date")
    ("ExpiryDate" "expiry-date")
    ("PostTypeId" "post-type-id")
    ("VoteTypeId" "vote-type-id")
    ("WikiPostId" "wiki-post-id")
    ("FlagTypeId" "flag-type-id")
    ("IsAnonymous" "is-anonymous")
    ("AnswerCount" "answer-count")
    ("IsUniversal" "is-universal")
    ("DisplayName" "display-name")
    ("OwnerUserId" "owner-user-id")
    ("BountyAmount" "bounty-amount")
    ("CreationDate" "creation-date")
    ("CommentCount" "comment-count")
    ("DeletionDate" "deletion-date")
    ("RevisionGuid" "revision-guid")
    ("MarkdownMini" "markdown-mini")
    ("ApprovalDate" "approval-date")
    ("TargetUserId" "target-user-id")
    ("LastEditDate" "last-edit-date")
    ("RejectionDate" "rejection-date")
    ("FavoriteCount" "favorite-count")
    ("RelatedPostId" "related-post-id")
    ("TargetTagName" "target-tag-name")
    ("ExcerptPostId" "excerpt-post-id")
    ("SourceTagName" "source-tag-name")
    ("LastAccessDate" "last-access-date")
    ("DeletionUserId" "deletion-user-id")
    ("LastAutoRename" "last-auto-rename")
    ("UserDisplayName" "user-display-name")
    ("SuggestedEditId" "suggested-edit-id")
    ("TargetRepChange" "target-rep-change")
    ("AutoRenameCount" "auto-rename-count")
    ("DeactivationDate" "deactivation-date")
    ("OwnerDisplayName" "owner-display-name")
    ("AcceptedAnswerId" "accepted-answer-id")
    ("LastActivityDate" "last-activity-date")
    ("ApprovedByUserId" "approved-by-user-id")
    ("PostNoticeTypeId" "post-notice-type-id")
    ("LastEditorUserId" "last-editor-user-id")
    ("CloseReasonTypeId" "close-reason-type-id")
    ("PostHistoryTypeId" "post-history-type-id")
    ("CommunityOwnedDate" "community-owned-date")
    ("CreationModeratorId" "creation-moderator-id")
    ("ApprovalModeratorId" "approval-moderator-id")
    ("PostNoticeDurationId" "post-notice-duration-id")
    ("DuplicateOfQuestionId" "duplicate-of-question-id")
    ("LastEditorDisplayName" "last-editor-display-name")
    ("DeactivationModeratorId" "deactivation-moderator-id")
    ("BelongsOnBaseHostAddress" "belongs-on-base-host-address")
    ("CloseAsOfftopicReasonTypeId" "close-as-offtopic-reason-type-id"))
  "A mapping of column names in CamelCase to snake-case.")

(defvar *patched-time* NIL)

(defun patch-time ()
  (unless *patched-time*
    (setf cl-postgres:*sql-readtable*
          (cl-postgres:copy-sql-readtable
           simple-date-cl-postgres-glue:*simple-date-sql-readtable*)
          *patched-time* t)))

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

;;; Row Mangling

(defun normalize-names (row)
  "Ensure columns are transformed from CamelCase to snake-case."
  (flet ((normalize (column)
           (let* ((name (first column))
                  (value (second column))
                  (snake-case-name (second (assoc name *camel->snake* :test #'string=))))
             (list (or snake-case-name name) value))))
    (loop for column in row while row
          collecting (normalize column))))

(defun clean-row (row)
  "Return a flattened list of columns and their values. Converts strings to numbers when needed."
  (mapcan (lambda (column)
            (let* ((name (nth-value 0 (read-from-string (first column))))
                   (value (second column))
                   (n (ignore-errors (parse-integer value))))
              (list `',name (or n value))))
          (normalize-names row)))

;;; Main

(defun build-query (table row)
  `(pg:query (:insert-into ',table :set ,@(clean-row row))))

(defun import-data (source &optional table)
  "Import data from SOURCE pathname. Optionally place data into TABLE using pomo:*database* as the database, otherwise returns data to standard output."
  (patch-time)
  (let ((data (fxml:make-source source))
        (handler (fxml.xmls:make-xmls-builder))
        (log-interval (local-time:timestamp+ (local-time:now) 5 :minute)))
    (log:info "Starting import now at ~a" (local-time:now))
    (loop for row = (grab-row data handler)
          until (end-of-document-p data)
          when row
          if (null table) collect row
          else do (handler-case (eval (build-query table row))
                    (pg-error:unique-violation () nil)
                    (pg-error:foreign-key-violation (c)
                      (log:error
                       "~%~A~%~%Query: ~A"
                       (pg:database-error-message c)
                       (pg:database-error-query c))))
          counting row into rows-processed
          do (fxml.klacks:peek-next data)
             (when (local-time:timestamp>= (local-time:now) log-interval)
               (log:info "Rows processed: ~d~%" rows-processed)
               (setf log-interval (local-time:timestamp+ (local-time:now) 5 :minute))))))

