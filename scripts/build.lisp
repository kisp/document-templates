;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-user)

(defvar *emerge-build* (probe-file "asd/"))
(when *emerge-build* (pushnew :emerge-build *features*))

#-emerge-build
(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))

#-emerge-build
(assert (equalp *default-pathname-defaults*
                (asdf:component-pathname (asdf:find-system :document-templates))))

#+emerge-build
(require 'asdf)

#+emerge-build
(asdf:initialize-source-registry
 `(:source-registry
   (:directory (,(uiop:getcwd) "asd"))
   :ignore-inherited-configuration))

(require :document-templates)

(setq document-templates::*template-directory* #p"/home/paul/unis/repos/ple/document-templates-data/")

(sb-ext:gc :full t)

(sb-ext:save-lisp-and-die "document-templates"
                          :toplevel #'document-templates::sbcl-main
                          :executable t
                          :compression #+sb-core-compression t #-sb-core-compression nil)
