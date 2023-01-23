;; mylib/utils/bottom-up-oc-test.lisp
;;;;

(in-package "AMP-DEV/TEST")

(in-suite :amp-dev-tests)

(defun get-this-file-path ()
  (if *load-pathname* *load-pathname*
    (asdf/component:component-pathname (asdf:find-system "amp-utils/test"))))

(defmacro with-dev-test1-fixture (&body body)
  `(let ((test-file (amp-pathnames:join-paths (get-this-file-path) "test/dev-test1.lisp"))
         (test-test-file (amp-pathnames:join-paths (get-this-file-path) "test/dev-test1-test.lisp"))
         (test-package-name "BOTTOM-UP-TEST-CONTEXT"))
     (unwind-protect
       (progn
         (format t "~&test-file: ~a~%" test-file)
         (format t "test-test-file: ~a~%" test-test-file)
         (load test-file)
         (if (find-package test-package-name)
             (print "package bottom-up-test-context found!")
           (print "no bottom-up-test-context package"))
         
         ,@body)

       (delete-package test-package-name)
       (delete-file test-test-file))))

(defun sym-from-fixture (sym-name)
  (intern sym-name (find-package "BOTTOM-UP-TEST-CONTEXT")))

(test get-file-path-from-description.test.1
  (with-dev-test1-fixture
   (is (equal #P"/temp/dev-test1.lisp"
              (get-file-path-from-description (sym-from-fixture "FUN2"))))))

(test add-data-to-function.test.1
  (with-dev-test1-fixture
    (add-data-to-function (sym-from-fixture "FUN1"))
    ;; verify that -test.lisp file is created
    (is-true (probe-file test-test-file))
    ;; verify that with-fn-data1 is added
    ;; verify that test-fn-data1 is added
    ))

