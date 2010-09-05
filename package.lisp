;;; document-templates
;;; Package definition
;;;

(defpackage :document-templates
  (:use :common-lisp :trivial-template :alexandria)
  (:export #:fill-template
	   #:show-default-parameters))
