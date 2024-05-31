(defpackage #:stack-overflow-builder
  (:use #:cl))
(in-package #:stack-overflow-builder)

(defparameter *input* #p"t/data/posts-500.xml")
(defparameter *input2* #p"t/data/badges-30.xml")
(defparameter *users* #p"t/data/users-100.xml")

(defvar *camel->snake*
  '(("CreationDate" "creation-date")
    ("DisplayName" "display-name")
    ("LastAccessDate" "last-access-date")
    ("WebsiteUrl" "website-url")
    ("AboutMe" "about-me")
    ("AccountId" "account-id")
    ("TagBased" "tag-based")
    ("UserId" "user-id"))
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
  `(pomo:query (:insert-into ',table :set ,@(clean-row row))))

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
          else do (eval (build-query table row))
          counting row into rows-processed
          do (fxml.klacks:peek-next data)
             (when (local-time:timestamp>= (local-time:now) log-interval)
               (log:info "Rows processed: ~d~%" rows-processed)
               (setf log-interval (local-time:timestamp+ (local-time:now) 5 :minute))))))

