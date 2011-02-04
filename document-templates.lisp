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

(defvar *template-directory*)

(defun list-template-directories ()
  (directory
   (merge-pathnames (make-pathname :name :wild :type :wild)
                    *template-directory*)))

(defun prompt-for-list-item (message list &key (key #'identity))
  (format t "~A~%" message)
  (loop for i upfrom 1
     for item in list
     do (format t "~A] ~A~%" i (funcall key item)))
  (let ((n (parse-integer (read-line) :junk-allowed t)))
    (or (and n (> n 0) (nth (1- n) list))
        (prompt-for-list-item message list :key key))))

(defun prompt-for-defaulted-string (message default)
  (format t "~A [~A]: " message default)
  (force-output)
  (let ((line (read-line)))
    (if (zerop (length line))
        default
        line)))

(defun main (argv)
  (sb-ext:disable-debugger)
  (when (= 1 (length argv))
    (format t "USAGE: document-templates TARGET-DIRECTORY~%")
    (format t "version ~A~%" #.(asdf:component-version (asdf:find-system :document-templates)))
    (sb-ext:quit :unix-status 1))
  (destructuring-bind (output-directory)
      (cdr argv)
    (let* ((output-directory (merge-pathnames (cl-fad:pathname-as-directory output-directory)))
           (template-directory (prompt-for-list-item
                                "Choose Template:"
                                (list-template-directories) :key #'directory-namestring))
           (template-config-pathname (merge-pathnames "config.lisp-expr" template-directory)))
      (let ((parameters (alist-plist
                         (with-open-config (config-parameters template-config-pathname)
                           (loop for (name . default) in
                                (plist-alist (getf config-parameters :default-parameters))
                              collect (cons name (prompt-for-defaulted-string name default)))))))
        (fill-template template-config-pathname
                       parameters
                       output-directory)))))
