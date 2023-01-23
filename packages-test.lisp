;; mylib/utils/packages-test.lisp


(defpackage "AMP-DEV/TEST"
  (:use "CL" "AMP-UTILS" "FIVEAM")
  (:import-from "AMP-DEV"
   "DEDUCE-AND-UPDATE-FILE-PATH"
   "GET-TEST-FILE-PATH"
   "DEDUCE-FILEPATH"
   "GET-FILE-PATH-FROM-DESCRIPTION"
   "ADD-DATA-TO-FUNCTION"))

(in-package "AMP-DEV/TEST")
(def-suite :amp-dev-tests)



(defpackage #:amp-utils/test
  (:use "CL" "FIVEAM" "AMP-UTILS"))

(in-package "AMP-UTILS/TEST")
(def-suite :utils-test)

