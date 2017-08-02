(setq *debugger-hook*
  (lambda (c h)
    (declare (ignore h))
    (format *error-output* "ERROR of type ~S:~%~A~%" (type-of c) c)
    (sb-ext:exit :code 1)))
(setq sb-ext:*invoke-debugger-hook* *debugger-hook*)

(defmacro without-warnings (&body body) 
  `(handler-bind ((warning #'muffle-warning)
		 #+sbcl (sb-ext:compiler-note #'muffle-warning))
    ,@body))
