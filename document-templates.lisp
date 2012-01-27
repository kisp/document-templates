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

(define-condition quit ()
  ((exit-code :initarg :exit-code :reader exit-code))
  (:default-initargs :exit-code 0))

(define-condition usage ()
  ((list-options :initarg :list-options :reader list-options)
   (errors :accessor errors :initarg :errors))
  (:default-initargs :list-options nil :errors nil))

(define-condition fatal (simple-error)
  ())

(defmacro with-quit-debug-handler (() &body body)
  `(let ((exit-code nil))
     (handler-case
         (progn ,@body
                (warn "~S should never return" ',body))
       (quit (c) (setq exit-code (exit-code c)))
       (error (c) (warn "caught~%~S~%  ~A~%where we expected QUIT condition" c c)))
     (format t "** exit-code: ~S~%" exit-code)))

(defun main2 (output-directory)
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
                     output-directory))))

(defun dispatch (opts args errs)
  (when errs
    (error 'usage :errors errs))
  (when (member :help opts)
    (error 'usage :list-options t))
  (when (member :version opts)
    (format t "document-templates, version ~A~%"
            #.(asdf:component-version (asdf:find-system :document-templates)))
    (error 'quit :exit-code 0))
  (when (member :templates-dir opts :key #'ensure-car)
    (let* ((arg (cadar (member :templates-dir opts :key #'ensure-car)))
           (dir (probe-file arg)))
      (cond ((not dir)
             (error 'fatal
                    :format-control "Does not exist: ~A"
                    :format-arguments (list arg)))
            ((not (cl-fad:directory-pathname-p dir))
             (error 'fatal
                    :format-control "Not a directory: ~A"
                    :format-arguments (list arg)))
            (t (setq *template-directory* dir)))))
  (when (member :list-templates opts)
    (dolist (dir (list-template-directories))
      (format t "~A~%" dir))
    (error 'quit :exit-code 0))
  (unless (length= 1 args)
    (error 'usage))
  (main2 (first args))
  (error 'quit :exit-code 0))

(defun main3 (argv)
  (let ((options
         (list
          (make-option '(#\l) '("list-templates")
                       (no-arg (constantly :list-templates))
                       "list template directories")
          (make-option nil '("templates-dir")
                       (req-arg (curry #'list :templates-dir) "DIR")
                       "override templates directory")
          (make-option '(#\V) '("version")
                       (no-arg (constantly :version))
                       "show version")
          (make-option '(#\h) '("help")
                       (no-arg (constantly :help))
                       "show help"))))
    (multiple-value-bind (opts args errs)
        (get-opt :permute options (cdr argv))
      (handler-case
          (dispatch opts args errs)
        (usage (c)
          (write-line (usage-line))
          (if (list-options c)
              (write-string
               (usage-info (format nil "Options:") options))
              (write-line (help-line)))
          (dolist (err (errors c))
            (write-string err))
          (error 'quit :exit-code 1))
        (fatal (c)
          (format t "Fatal error: ")
          (apply #'format t
                 (simple-condition-format-control c)
                 (simple-condition-format-arguments c))
          (terpri)
          (error 'quit :exit-code 1))))))

(defun usage-line ()
  "Usage: document-templates TARGET-DIRECTORY")

(defun help-line ()
  "To list options, try the `--help' option.")

#+sbcl
(defun main (argv)
  (sb-ext:disable-debugger)
  (handler-case
      (main3 argv)
    (quit (c) (sb-ext:quit :unix-status (exit-code c)))))
