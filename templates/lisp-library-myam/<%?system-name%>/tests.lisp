;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package :<%?system-name%>)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package :myam))

(defsuite* :<%?system-name%>-test)

(deftest dummy
  (is (= 1 1)))
