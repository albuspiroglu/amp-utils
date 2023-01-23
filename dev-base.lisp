;; mylib/utils/dev-base.lisp


(in-package "AMP-DEV-BASE")


(defvar *maturity-levels*
  '((0 . "Compiles")
    (1 . "Runs default case")
    (2 . "Tested with edge cases")
    (3 . "Contracts exist")
    (4 . "Documented")))

(defmacro defunq (name args (&key maturity documentation) &body body)
  "Define a function with a maturity value in the symbol's plist.

**e.g.**
    (defunq fn ()
        (:maturity 3
         :documentation \"explain what this function does\")
      (print \"test\"))
"
;;;   `(prog1
;;;      (make-symbol ,name)
;;;      (defun ,name ,args ,documentation ,@body)
;;;      (setf (get ,name :maturity) ,maturity)))
;;;      
  (setf (symbol-plist name) (list :maturity maturity))
  `(defun ,name ,args ,documentation ,@body))


#|
How about the form below to define things?
E.g. to define a function, one chooses (:type function)
also, the form creates a closure that binds certain lexical variables,
  such as "this-function" "this-symbol", etc.

(def                                     ;; or define
  (:type     function)
  (:header   test-fn (arg1 arg2))
  (:maturity 1)
  (:arbitrary-prop-1 'value1)
  (:another-prop 33)
  (:body
   (format t "hello from function object: ~a~%" this-function)
   (format t "my prop-list: ~{~a~}~%" (symbol-plist this-symbol)))

then, next thing is to redefine defun to extend to the above form ;)

Annotations can also be used:

@(:maturity 1)
@(:arbitrary-prop-1 'value)
@(:another-prop 33)
(defun test-fn (a b)
  (format t "hello from fn: ~a~%" (symbol-name 'test-fn))
  (format t "my prop-list: (~{~a ~})~%" (symbol-plist 'test-fn)))
  
 
|#

(defmacro def () nil)
