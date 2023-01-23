;; mylib/utils/test/dev-test1.lisp
;;;;
;;;; test functions to use from bottom-up-oc.lisp
;;;; bottom-up has functions to create corresponding dev-test1-test.lisp
;;;; file and corresponding test forms for the forms in this file.
;;;; the dev-test1-test.lisp file will be created on the fly during
;;;; test runs, and be deleted after each test.


(defpackage #:bottom-up-test-context
  (:use #:cl))

(in-package #:bottom-up-test-context)

(defun fun1 (arg1 arg2)
  (+ arg1 arg2))

(defun fun2 ()
  "fun2 description, to test get-file-path-from-description function.

Source file: \"/temp/dev-test1.lisp\"
"
  nil)
