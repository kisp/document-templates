;;; -*- mode: lisp -*-
;;; document-templates
;;; ASDF system definition
;;;
(asdf:defsystem :document-templates
  :name "document-templates"
  :version #.(with-open-file
		 (vers (merge-pathnames "version.lisp-expr" *load-truename*))
	       (read vers))
  :components ((:static-file "version" :pathname #p"version.lisp-expr")
	       (:file "package")
	       (:file "document-templates" :depends-on ("package"))
	       )
  :depends-on (:alexandria :cl-fad :trivial-template))
