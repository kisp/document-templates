;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(defpackage #:<%?system-name%>
  (:nicknames #:uh)
  (:use #:cl #:alexandria))

(in-package #:<%?system-name%>)

(defun format-usage ()
  (format t "usage: <%?system-name%> [-Vh] [ARGS]~%"))

(defun usage-and-quit (&optional message &rest args)
  (when message
    (apply #'format t message args)
    (terpri)
    (terpri))
  (format-usage)
  (sb-ext:quit :unix-status 1))

(defun main (argv)
  (sb-ext:disable-debugger)
  (multiple-value-bind (args opts err)
      (cffi-getopt:getopt argv "Vhf:")
    (when err (usage-and-quit))
    (cond
      ((assoc #\V opts)
       (format t "<%?system-name%> version ~A~%"
               (asdf:component-version (asdf:find-system :<%?system-name%>))))
      ((assoc #\h opts)
       (format-usage))
      (args (destructuring-bind (arg) args
              (write-string-into-file
               (ppcre:regex-replace-all "zwei" (read-file-into-string arg) "2")
               (format nil "~A-new" arg))))
      (t
       (usage-and-quit "no input file specified")))))
