;; mylib/utils/packages.lisp

(defpackage #:amp-dev-base
  (:use :cl)
  (:export
   "DEFUNQ"
   "*MATURITY-LEVELS*"
))


(defpackage #:amp-utils
  (:use "CL" "AMP-DEV-BASE")
  (:export 
   "=APPLY"
   "=BIND"
   "=DEFUN"
   "=FUNCALL"
   "=LAMBDA"
   "=VALUES"
   "AAND"
   "ACOND"
   "ACOND2"
   "AFTER"
   "AIF"
   "AIF2"
   "ALAMBDA"
   "ALWAYS"
   "APPEND-STRING"
   "AWHEN"
   "AWHEN2"
   "AWHILE"
   "AWHILE2"
   "BEFORE"
   "BEST"
   "CONJOIN"
   "CONT-LEXICAL"
   "CURRENT-SYMBOLS"
   "CURRY"
   "DEFLEXICAL"
   "DEFMACRO!"
   "DEFMACRO/G!"
   "DISJOIN"
   "DO-FILE"
   "DUPLICATE"
   "FACT"
   "FILTER"
   "FLATTEN"
   "FOR"
   "G!-SYMBOL-P"
   "GET-NUMS"
   "GET-STATS"
   "GROUP"
   "IT"
   "LIST-PACKAGE-OWNED-SYMBOLS"
   "LREC"
   "MAP->"
   "MAP-INT"
   "MAP-LAZY"
   "MAP0-N"
   "MAP1-N"
   "MAPA-B"
   "MAPCARS"
   "MAPPEND"
   "MAPPEND"
   "MEMOIZE"
   "MKLIST"
   "MKSTR"
   "MOST"
   "MOSTN"
   "NPUSH"
   "O!-SYMBOL-P"
   "PMACROEXPAND"
   "PMACROEXPAND-1"
   "PRUNE"
   "PULL"
   "RCURRY"
   "READ2"
   "RMAPCAR"
   "SELF"
   "SPLIT-IF"
   "SYMB"
   "WITH-GENSYMS"
   "DEFUN-IN-PACKAGE"
   "WHEN-NONE-OF"
   "GET-NEW-ITEM-NAME"
   "STRING-APPEND"
   "PRINT-OBJECT-TO-STRING"
   "STRING-REMOVE-ESCAPES"
   "MATCH-AREF"))


(defpackage #:amp-pathnames
  (:documentation "This code is mostly from Peter Seibel's pathnames chapter,
                   then some additions as I needed more utilities.")
  (:use "CL" "AMP-UTILS" "AMP-DEV-BASE")
  (:export
   "LIST-DIRECTORY"
   "FILE-EXISTSP"
   "DIRECTORY-PATHNAMEP"
   ; WE DONT' HAVE THIS?:FILE-PATHNAMEP
   "PATHNAME-AS-DIRECTORY"
   "PATHNAME-AS-FILE"
   "WALK-DIRECTORY"
   "DIRECTORYP"
   "FILEP"
   "JOIN-PATHS"
   "APPEND-PATHS"
   "GET-UNIQUE-PATH"))


(defpackage #:amp-scripts
  (:use "CL")
  (:export
   "CREATE-SYMLINKS-TO-ITEMS"))


(defpackage #:amp-dev
  (:nicknames #:dev)
  (:use "CL" "AMP-UTILS")
  (:export
   "ADD-DATA-TO-FUNCTION"
   "DEF-MACRO-TEST"
   "DEFCLASS-TEST"
   "DEFUNQ"
))



(defpackage #:amp
  (:use 
   "CL"
   "AMP-UTILS"
   "AMP-PATHNAMES"
   "AMP-SCRIPTS"
   "AMP-DEV")

  (:export
   ;; amp-utils
   "=APPLY"         "=BIND"   "=DEFUN"    "=FUNCALL"      "=LAMBDA"           "=VALUES"   "AAND"       
   "ACOND"          "ACOND2"  "AFTER"     "AIF"           "AIF2"              "ALAMBDA"   "FACT"
   "ALWAYS"         "AWHEN"   "AWHEN2"    "AWHILE"        "AWHILE2"           "BEFORE"    "BEST"
   "CONJOIN"        "CONT-LEXICAL"        "CURRENT-SYMBOLS"                   "CURRY"     "DEFLEXICAL"
   "DEFMACRO!"      "DEFMACRO/G!"         "DISJOIN"           "DO-FILE"   "DUPLICATE"
   "FILTER"         "FLATTEN" "FOR"       "G!-SYMBOL-P"   "GET-NUMS"          "GET-STATS" "GROUP"     
   "IT"             "LIST-PACKAGE-OWNED-SYMBOLS"          "LREC"              
   "MAP->"          "MAP-INT" "MAP-LAZY"  "MAP0-N"        "MAP1-N"            "MAPA-B"    "MAPCARS"       
   "MAPPEND"        "MAPPEND"             "MEMOIZE"
   "MKLIST"         "MKSTR"   "MOST"      "MOSTN"         "NPUSH"             "O!-SYMBOL-P"   
   "PMACROEXPAND"   "PMACROEXPAND-1"      "PRUNE"         "PULL"              "RCURRY"    "READ2"
   "RMAPCAR"        "SELF"                "SPLIT-IF"      "SYMB"              "WITH-GENSYMS"
   "DEFUN-IN-PACKAGE"                     "WHEN-NONE-OF"  "GET-NEW-ITEM-NAME" 
   "STRING-APPEND"  "PRINT-OBJECT-TO-STRING"              "STRING-REMOVE-ESCAPES"
   "MATCH-AREF"

   ;; amp-pathnames
   "LIST-DIRECTORY"   "FILE-EXISTSP"   "DIRECTORY-PATHNAMEP"   ; WE DONT' HAVE THIS?:FILE-PATHNAMEP
   "PATHNAME-AS-DIRECTORY"             "PATHNAME-AS-FILE"
   "WALK-DIRECTORY"   "DIRECTORYP"     "FILEP"                "JOIN-PATHS"    "APPEND-PATHS"
   "GET-UNIQUE-PATH"

   ;; amp-scripts
   "CREATE-SYMLINKS-TO-ITEMS"    

   ;; amp-dev
   "ADD-DATA-TO-FUNCTION"       "DEF-MACRO-TEST"       "DEFCLASS-TEST"        "DEFUNQ"
   
   ;; amp
   "DELETE-SYSTEM"
))

(in-package :amp)

(defun delete-system ()
  (in-package "CL-USER")
  (dolist (p '("AMP" "AMP-PATHNAMES" "AMP-SCRIPTS" "AMP-DEV" "AMP-UTILS" "AMP-DEV-BASE"))
    (delete-package p))
  (asdf/system-registry:clear-system "amp-utils"))

