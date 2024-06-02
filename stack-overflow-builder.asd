;;;; stack-overflow-builder.asd

(asdf:defsystem #:stack-overflow-builder
  :description "Describe stack-overflow-builder here"
  :author "zyd"
  :license  "zlib"
  :version "0.0.1"
  :serial t
  :depends-on (#:fxml #:postmodern #:simple-date/postgres-glue #:local-time #:log4cl)
  :components ((:file "stack-overflow-builder")))
