(defpackage #:stack-overflow-builder
  (:use #:cl))
(in-package #:stack-overflow-builder)

(defparameter *test-input* #p"/run/media/zyd/d5e258d8-31b6-4020-8caf-2ff4a81a8523/Archives/data/stackexchange-2024-04-02/test.xml")

(defun collect-rows (input)
  (let ((source (fxml:make-source input))
        (handler (fxml.xmls:make-xmls-builder)))
    (loop for row = (row source)
          until (end-of-document-p source)
          when row
            collect (fxml.klacks:serialize-element source handler)
          do (fxml.klacks:peek-next source))))

;; (defun collect-rows (input)
;;   (let ((source (cxml:make-source input))
;;         (handler (cxml-xmls:make-xmls-builder))
;;         (posts '()))
;;     (loop for element = (rowp source)
;;           until (end-of-document-p source)
;;           do (when element
;;                (push (klacks:serialize-element source handler) posts))
;;              (klacks:peek-next source))
;;     posts))

(defun row (source)
  (multiple-value-bind (type namespace local-name qualified-name)
      (fxml.klacks:peek source)
    (declare (ignore namespace local-name))
    (when (and (eq type :start-element)
               (string= qualified-name "row"))
      type)))

(defun end-of-document-p (source)
  (eq (fxml.klacks:peek source) :end-document))
