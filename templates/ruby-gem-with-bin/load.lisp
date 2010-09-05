;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:oos 'asdf:load-op :cl-ppcre))

(defpackage #:ruby-gem
  (:use #:cl #:document-templates #:cl-ppcre))

(in-package #:ruby-gem)

(defun current-date ()
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time (get-universal-time))
    (declare (ignore second minute hour))
    (format nil "~4,'0D-~2,'0D-~2,'0D" year month date)))

(defun tokenize-gem-name (gem-name)
  (mapcar #'string-downcase (split "(-|_|(?<=[a-z])(?=[A-Z]))" gem-name)))

(defun join (list-of-strings char)
  "CHAR can be nil"
  (with-output-to-string (out)
    (loop for tail on list-of-strings
	 do (write-string (car tail) out)
	 when (and (cdr tail) char)
	 do (write-char char out))))

(defun underscores (string)
  (join (tokenize-gem-name string) #\_))

(defun camel-case (string)
  (join (mapcar #'string-capitalize (tokenize-gem-name string)) nil))
