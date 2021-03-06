;;; document-templates

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

;;; xxx not pure - *default-pathname-defaults*
(defun template-files-directory (config-parameters)
  (let ((template-files-directory
         (getf config-parameters :template-files-directory)))
    (non-wild-directory
     (if template-files-directory
         (merge-pathnames template-files-directory)
         *default-pathname-defaults*))))

;;; xxx how does this work?
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

(defun check-for-existing-dirs (parameters template-file-pathnames output-directory)
  (dolist (dir (remove-duplicates
                (mapcar
                 (lambda (template-file-pathname)
                   (labels ((just-first-directory (pathname)
                              (make-pathname
                               :name nil :type nil
                               :directory (subseq (pathname-directory pathname) 0 2)
                               :defaults pathname)))
                     (merge-pathnames
                      (just-first-directory
                       (fill-pathname template-file-pathname parameters))
                      output-directory)))
                 template-file-pathnames)
                :test #'equal))
    (when (probe-file dir)
      (error 'fatal
             :format-control "already exists ~S"
             :format-arguments (list dir)))))

(defun fill-template (template-config-pathname parameters output-directory)
  (with-open-config (config-parameters template-config-pathname)
    (let* ((output-directory
            (non-wild-directory output-directory))
           (template-files-directory
            (template-files-directory config-parameters))
           (template-file-pathnames
            (template-file-pathnames config-parameters
                                     template-files-directory))
           (merged-parameters (merge-parameters parameters config-parameters)))
      (load-template-lisp-init-file config-parameters)
      (check-for-existing-dirs merged-parameters template-file-pathnames output-directory)
      (with-template-package (config-parameters)
        (dolist (template-file-pathname template-file-pathnames)
          (fill-template-file template-file-pathname
                              merged-parameters
                              output-directory))))))

(defun choose-template (template-name)
  (if template-name
      (or (find-if (lambda (dir)
                  (equal template-name
                         (car (last (pathname-directory dir)))))
                   (list-template-directories))
          (error 'fatal
                 :format-control "no template found with name ~S"
                 :format-arguments (list template-name)))
      (prompt-for-list-item
       "Choose Template:"
       (list-template-directories) :key #'directory-namestring)))

(defun get-template-parameters (template-config-pathname template-parameters-file)
  (let (parameters-file-content)
    (labels ((parameters-file-content ()
               (or parameters-file-content
                   (setq parameters-file-content
                         (progn
                           (unless (probe-file template-parameters-file)
                             (error 'fatal
                                    :format-control "file does not exist ~S"
                                    :format-arguments (list template-parameters-file)))
                           (read-config-file template-parameters-file)))))
             (value-for-name-via-prompt (name default)
               (prompt-for-defaulted-string name default))
             (value-for-name-from-file (name default)
               (getf (parameters-file-content) name default)))
      (let ((value-for-name
             (if template-parameters-file
                 #'value-for-name-from-file
                 #'value-for-name-via-prompt)))
        (alist-plist
         (with-open-config (config-parameters template-config-pathname)
           (loop for (name . default) in
                (plist-alist (getf config-parameters :default-parameters))
              collect (cons name (funcall value-for-name name default)))))))))

(defvar *template-directory*)

(defun list-template-directories ()
  (remove-if
   (lambda (dir)
     (or (string= ".git"
                  (file-namestring (cl-fad:pathname-as-file dir)))
         (not (cl-fad:directory-pathname-p dir))
         (not (probe-file (merge-pathnames "config.lisp-expr" dir)))))
   (directory
    (merge-pathnames (make-pathname :name :wild :type :wild)
                     *template-directory*)
    #+sbcl :resolve-symlinks #+sbcl nil)))

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

(define-condition usage (quit)
  ((list-options :initarg :list-options :reader list-options)
   (errors :accessor errors :initarg :errors))
  (:default-initargs :list-options nil :errors nil :exit-code 1))

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

(define-condition unexpected-args (error)
  ((args :reader unexpected-args :initarg :args)))

(define-condition unexpected-key (error)
  ((key :reader unexpected-key-key :initarg :key)
   (value :reader unexpected-key-value :initarg :value)))

(define-condition missing-args (error)
  ((args :reader missing-args :initarg :args)))

(defmacro defcmd (name lambda-list &body body)
  `(progn
     (setf (get ',name 'parsed-lambda-list)
           ',(multiple-value-list
              (parse-ordinary-lambda-list lambda-list)))
     (defun ,name ,lambda-list ,@body)))

(defun check-cmd-application (parsed-lambda-list args opts)
  (destructuring-bind (required optional rest keys allow-other-keys aux key-exists)
      parsed-lambda-list
    (declare (ignore aux key-exists))
    (cond
      (rest)
      ((<= (length required)
           (length args)
           (+ (length required) (length optional))))
      ((< (length args)
          (length required))
       (error 'missing-args :args (subseq required (length args))))
      (t (error 'unexpected-args
                :args (subseq args (+ (length required) (length optional))))))
    (unless allow-other-keys
      (loop for (key . value) in (alexandria:plist-alist opts)
         unless (member key keys :key #'caar)
         do (error 'unexpected-key :key key :value value)))))

(defun cmdcall (cmd args opts)
  (let ((parsed-lambda-list (get cmd 'parsed-lambda-list :not-found)))
    (when (eql :not-found parsed-lambda-list)
      (error "~S is not a known cmd" cmd))
    (check-cmd-application parsed-lambda-list args opts)
    (apply cmd (append args opts))))

(defun invoke-with-templates-dir-opt (templates-dir fn)
  (let ((dir (probe-file templates-dir)))
    (cond ((not dir)
           (error 'fatal
                  :format-control "Does not exist: ~A"
                  :format-arguments (list templates-dir)))
          ((not (cl-fad:directory-pathname-p dir))
           (error 'fatal
                  :format-control "Not a directory: ~A"
                  :format-arguments (list templates-dir)))
          (t (let ((*template-directory* dir))
               (funcall fn))))))

(defmacro with-templates-dir-opt ((templates-dir) &body body)
  `(invoke-with-templates-dir-opt ,templates-dir (lambda () ,@body)))

(defcmd cmd-help (&rest args &key &allow-other-keys)
  (declare (ignore args))
  (error 'usage :list-options t :exit-code 0))

(defcmd cmd-null (&rest args &key &allow-other-keys)
  (declare (ignore args))
  (error 'usage :list-options t :exit-code 1))

(defcmd cmd-version (&rest args &key &allow-other-keys)
  (declare (ignore args))
  (format t "document-templates, version ~A~%"
          #.(asdf:component-version (asdf:find-system :document-templates)))
  (error 'quit :exit-code 0))

(defcmd cmd-list-templates (&key (templates-dir *template-directory*))
  (with-templates-dir-opt (templates-dir)
    (dolist (dir (list-template-directories))
      (format t "~A~%" dir))
    (error 'quit :exit-code 0)))

(defcmd cmd-fill-template (output-directory
                           &key
                           (templates-dir *template-directory*)
                           template-name
                           template-parameters-file)
  (with-templates-dir-opt (templates-dir)
    (let* ((output-directory (merge-pathnames (cl-fad:pathname-as-directory output-directory)))
           (template-directory (choose-template template-name))
           (template-config-pathname (merge-pathnames "config.lisp-expr" template-directory)))
      (fill-template template-config-pathname
                     (get-template-parameters template-config-pathname
                                              template-parameters-file)
                     output-directory))
    (error 'quit :exit-code 0)))

(defun dispatch (opts args errs)
  (when errs
    (error 'usage :errors errs))
  (when (< 1 (count-if #'symbolp opts))
    (error 'fatal
           :format-control "More than one command given."))
  (let ((cmd (or (find-if #'symbolp opts)
                 'cmd-null)))
    (handler-case
        (cmdcall cmd args (flatten (remove cmd opts)))
      (missing-args (c)
        (error 'fatal :format-control "Command ~A expected arg~P ~{~A~^, ~}."
               :format-arguments (list cmd (length (missing-args c)) (missing-args c))))
      (unexpected-args (c)
        (error 'fatal :format-control "Command ~A did not expect arg~P ~{~A~^, ~}."
               :format-arguments (list cmd (length (unexpected-args c)) (unexpected-args c))))
      (unexpected-key (c)
        (error 'fatal :format-control "Command ~A did not expect option ~A."
               :format-arguments (list cmd (unexpected-key-key c)))))))

(defun main (argv)
  (when (sb-ext:posix-getenv "DOCUMENT_TEMPLATES_TEMPLATE_DIRECTORY")
    (setq *template-directory*
          (sb-ext:posix-getenv "DOCUMENT_TEMPLATES_TEMPLATE_DIRECTORY")))
  (let* ((cmds
          (list
           (make-option '(#\f) '("fill-template")
                        (no-arg (constantly 'cmd-fill-template))
                        "Fill a template")
           (make-option '(#\l) '("list-templates")
                        (no-arg (constantly 'cmd-list-templates))
                        "List template directories")
           (make-option '(#\V) '("version")
                        (no-arg (constantly 'cmd-version))
                        "Show version")
           (make-option '(#\h) '("help")
                        (no-arg (constantly 'cmd-help))
                        "Show help")))
         (options
          (list
           (make-option '(#\T) '("templates-dir")
                        (req-arg (curry #'list :templates-dir) "DIR")
                        "Override templates directory")
           (make-option '() '("template-name")
                        (req-arg (curry #'list :template-name) "NAME")
                        "Template to apply")
           (make-option '() '("template-parameters")
                        (req-arg (curry #'list :template-parameters-file) "FILE")
                        "File to read for template parameters")))
         (all (append cmds options)))
    (multiple-value-bind (opts args errs)
        (get-opt :permute all (cdr argv))
      (handler-case
          (dispatch opts args errs)
        (usage (c)
          (write-line (usage-line))
          (if (list-options c)
              (progn
                (write-string
                 (usage-info (format nil "Commands:") cmds))
                (write-string
                 (usage-info (format nil "Options:") options)))
              (write-line (help-line)))
          (dolist (err (errors c))
            (write-string err))
          (error 'quit :exit-code (exit-code c)))
        (fatal (c)
          (format t "Fatal error: ")
          (apply #'format t
                 (simple-condition-format-control c)
                 (simple-condition-format-arguments c))
          (terpri)
          (error 'quit :exit-code 1))))))

(defun usage-line ()
  "Usage: document-templates [OPTS]")

(defun help-line ()
  "To list options, try the `--help' option.")

#+sbcl
(defun sbcl-main ()
  (sb-ext:disable-debugger)
  (handler-case
      (main sb-ext:*posix-argv*)
    (quit (c) (sb-ext:exit :code (exit-code c)))))

(defun dump ()
  (asdf:clear-configuration)
  (asdf/system-registry:clear-registered-systems)
  (sb-ext:save-lisp-and-die "document-templates"
                            :toplevel #'document-templates::sbcl-main
                            :executable t
                            :save-runtime-options t
                            :compression
                            #+sb-core-compression t
                            #-sb-core-compression nil))
