;;; document-templates
;;;
(in-package :document-templates)

(defun pathname-parent-directory (pathname)
  (make-pathname :name nil :type nil
		 :defaults (cl-fad:pathname-as-file pathname)))

(defun non-wild-pathname (thing)
  (let ((pathname (pathname thing)))
    (assert (not (wild-pathname-p pathname)))
    pathname))

(defun non-wild-directory (thing)
  (let ((pathname (pathname thing)))
    (assert (not (wild-pathname-p pathname)))
    (assert (cl-fad:directory-pathname-p pathname))
    pathname))

(defun enough-pathname (pathname)
  (pathname (enough-namestring pathname)))

(defun read-config-file (pathname)
  (with-open-file (in pathname :external-format :utf-8)
    (read in)))

(defun template-files-directory (config-parameters)
  (let ((template-files-directory
	 (getf config-parameters :template-files-directory)))
    (non-wild-directory
     (if template-files-directory
	 (merge-pathnames template-files-directory)
	 *default-pathname-defaults*))))

(defun template-file-pathnames (config-parameters template-files-directory)
  (mapcar #'enough-pathname
	  (let ((*default-pathname-defaults* template-files-directory))
	    (delete-duplicates
	     (mapcan #'directory
		     (getf config-parameters :template-file-pathnames))
	     :test #'equal))))

(defun template-package (config-parameters)
  (let ((package-designator
	 (getf config-parameters :template-package
	       :document-templates)))
    (find-package package-designator)))

(defmacro with-template-package ((config-parameters) &body body)
  `(let ((*package* (template-package ,config-parameters)))
     ,@body))

(defun load-template-lisp-init-file (config-parameters)
  (when-let ((init-file (getf config-parameters :template-lisp-init-file)))
    (load init-file :verbose t)))

(defun fill-pathname (pathname parameters)
  (flet ((transform (obj)
	   (typecase obj
	     (string (princ-template-to-string (parse-template obj)
					       parameters))
	     (t obj))))
    (make-pathname :name (transform (pathname-name pathname))
		   :type (transform (pathname-type pathname))
		   :directory (mapcar #'transform
				      (pathname-directory pathname))
		   :defaults pathname)))

(defun fill-template-file (template-file-pathname parameters output-directory)
  (let ((output-pathname (merge-pathnames (fill-pathname template-file-pathname
							 parameters)
					  output-directory)))
    (ensure-directories-exist output-pathname)
    (with-open-file (out output-pathname :direction :output
			 :external-format :utf-8 :if-exists :supersede)
      (princ-template (parse-template
		       (read-file-into-string template-file-pathname
					      :external-format :utf-8))
		      parameters
		      out))))

(defun merge-parameters (parameters config-parameters)
  (append parameters (getf config-parameters :default-parameters)))

(defmacro with-open-config ((config-parameters-var template-config-pathname)
			    &body body)
  (check-type config-parameters-var symbol)
  (with-gensyms (=template-config-pathname=)
    `(let* ((,=template-config-pathname= ,template-config-pathname)
	    (*default-pathname-defaults*
	     (pathname-parent-directory ,=template-config-pathname=))
	    (,config-parameters-var
	     (read-config-file ,=template-config-pathname=)))
       ,@body)))

(defun show-default-parameters (template-config-pathname)
  (with-open-config (config-parameters template-config-pathname)
    (loop for (name . default) in
	 (plist-alist (getf config-parameters :default-parameters))
	 do (format t "; ~S~24T~S~%" name default))))

(defun fill-template (template-config-pathname parameters output-directory)
  (with-open-config (config-parameters template-config-pathname)
    (let* ((output-directory
	    (non-wild-directory output-directory))
	   (template-files-directory
	    (template-files-directory config-parameters))
	   (template-file-pathnames
	    (template-file-pathnames config-parameters
				     template-files-directory)))
      (load-template-lisp-init-file config-parameters)
      (with-template-package (config-parameters)
	(dolist (template-file-pathname template-file-pathnames)
	  (fill-template-file template-file-pathname
			      (merge-parameters parameters config-parameters)
			      output-directory))))))
