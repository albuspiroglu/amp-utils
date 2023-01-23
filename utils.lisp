;; mylib/utils/utils.lisp

(in-package :amp-utils)

#|
Ideas:
* improve swank:fuzzy-completions with searching in the documentation of symbols as well.
  call it something like: (everything name)
  (Note that I've done this within lib-helper library)

* implement todo items in this page



|#


;; TODO destructuring bind into structs or objects:
#|
Just like destructuring bind binds a list of items into variables,
we should try binding a list of items into structures or objects.
(hindsight: Wrong conclusion): This seems to solve (mostly) the need for editor's type translation
based on visual selections.
  (N.B. the actual difficulty is aligning the structures, not assigning the corresponding places as
        destructuring does. Destructuring part is more trivial.)
|#

(defmacro pmacroexpand (expr)
  "pprint and macroexpand together macro"
  `(pprint (macroexpand ',expr)))


(defmacro pmacroexpand-1 (expr)
  "pprint and macroexpand1 together macro"
  `(pprint (macroexpand-1 ',expr)))

   
(defunq list-package-owned-symbols (&optional (package *package*))
    (:maturity
     1
     :documentation
     "List symbols originating in / defined and bound in given package")
  (let ((imported-symbols (mapcan (lambda (pkg)
                                    (let (rez)
                                      (do-symbols (s pkg rez)
                                        (push s rez))))
                                  (package-use-list (find-package package)))))
    (do-symbols (s package)
      (when (and (boundp s)
                 (not (member s imported-symbols)))
        (print s)))))


(defun single? (lst)
  (and (consp lst) (null (cdr lst))))

(defun append1 (lst obj)
  (append lst (list obj)))

(defun map-int (fn n)
  "Run fn with numbers up to n"
  (let ((acc nil))
    (dotimes (i n)
      (push (funcall fn i) acc))
    (nreverse acc)))

(defvar map-numbers nil "Alias for map-int function")
(setf (symbol-function 'map-numbers) (function map-int))


(defun map-lazy (fn ls n)
  "map fn to ls up to n items"
  (loop for i in ls
        for cnt from 1 to n
        collect (funcall fn i)))

(defun filter (fn lst)
  "Accumulate elements that fn returns a value for items in lst"
  (let ((acc nil))
    (dolist (x lst)
      (let ((val (funcall fn x)))
        (if val (push val acc))))
    (nreverse acc)))

(defun most (fn lst)
  "Returns two values: 
the element of a list with the highest score,
   according to some scoring function, 
and the score"
  (if (null lst)
      (values nil nil)
    (let* ((wins (car lst))
           (max (funcall fn wins)))
      (dolist (obj (cdr lst))
        (let ((score (funcall fn obj)))
          (when (> score max)
            (setf wins obj
                  max score))))
      (values wins max))))

(defun combiner (x)
  (typecase x
    (number #'+)
    (list	#'append)
    (t		#'list)))

(defun combine (&rest args)
  (apply (combiner (car args))
         args))

(defun compose (&rest fns)
  "Takes one or more functions and returns a new function
in which all of them are applied in succession."
  (destructuring-bind (fn1 . rest) (reverse fns)
    (lambda (&rest args)
      (reduce (lambda (v f) (funcall f v))
              rest
              :initial-value (apply fn1 args)))))

#|
cddr 	= (compose #'cdr #'cdr)
nth	 	= (compose #'car #'nthcdr)
atom	= (compose #'not #'consp)
        = (rcurry #'typep 'atom)
<=		= (disjoin #'< #'=)
listp	= (disjoin #'null #'consp)
        = (rcurry #'typep 'list)
1+		= (curry #'+ 1)
        = (rcurry #'+ 1)
1-      = (rcurry #'- 1)
mapcan	= (compose (curry #'apply #'nconc) #'mapcar)
complement = (curry #'compose #'not)
|#

(defun disjoin (fn &rest fns)
  "Take one or more predicates, and return a predicate that
returns true when any of the predicates return true."
  (if (null fns)
      fn
    (let ((disj (apply #'disjoin fns)))
      (lambda (&rest args)
        (or (apply fn args) (apply disj args))))))

(defun conjoin (fn &rest fns)
  "Returns a predicate that returns true when all of the fns
(predicates) return true."
  (if (null fns)
      fn
    (let ((conj (apply #'conjoin fns)))
      (lambda (&rest args)
        (and (apply fn args) (apply conj args))))))

(defun curry (fn &rest args)
  "Return a function that expects rest of the arguments
to fn after args."
  (lambda (&rest args2)
    (apply fn (append args args2))))

(defun rcurry (fn &rest args)
  "Return a function that expects rest of the arguments
to fn before args, i.e. args is the last argument to fn afterwards."
  (lambda (&rest args2)
    (apply fn (append args2 args))))

(defun always (x)
  "Same as constantly function."
  (lambda (&rest args) x))

(defun get-stats (lst)
  "Return an alist of stats.
dev is mean average"
  (let ((cnt 0)
        (min (first lst))
        (max (first lst))
        avg
        (total 0)
        (total-mean-dev 0)
        mean-dev)
    (mapcar (lambda (x)
              (incf total x)
              (incf cnt)
              (when (< min x)
                (setq min x))
              (when (> max x)
                (setq max x)))
            lst)
    (setq avg (/ total cnt))
    (mapcar (lambda (x)
              (incf total-mean-dev (abs (- x avg))))
            lst)
    (setq mean-dev (/ total-mean-dev cnt))
    (list :cnt cnt
          :min min
          :max max
          :avg avg
          :mean-dev mean-dev)))

;; todo: I should make it easier to form such data:
#|
(let ((lst (append (mapcar (lambda (x) (first (cadr x)))
								  comps)
                   (mapcar (lambda (x) (second (cadr x)))
								  comps))))
		  (cl-user::get-stats minmax))
|#


(defun mkstr (&rest args)
  "Return a string concatenation of args"
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))


(defun symb (&rest args)
  "Return an interned symbol of concatenated args"
  (values (intern (apply #'mkstr args))))


(defmacro deflexical (var val)
  "Define a global variable that is not special,
i.e. with lexical binding."
  (let ((var-special (gensym)))
    `(progn
       (defvar ,var-special ,val)
       (define-symbol-macro ,var ,var-special))))
 


(defun group (source n)
  "Given that source is a list of items, group them in sublists of n length.
This mirrors operators like setf and psetf that already group arguments
by 2, to structure related data."
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
             (let ((rest (nthcdr n source)))
               (if (consp rest)
                   (rec rest (cons
                              (subseq source 0 n)
                              acc))
                 (nreverse
                  (cons source acc))))))
    (if source (rec source nil) nil)))


(defun flatten (x)
  "Given an arbitrarily nested list structure, flatten will return
new list containing all the atoms reachable through that list structure."
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   #+sbcl
                   ((typep x 'sb-impl::comma) (rec (sb-impl::comma-expr x) acc))
                   ((atom x) (cons x acc))
                   (t (rec
                       (car x)
                       (rec (cdr x) acc))))))
    (rec x nil)))


(defun g!-symbol-p (s)
  (and (symbolp s)
       (> (length (symbol-name s)) 2)
       (string= (symbol-name s)
                "G!"
                :start1 0
                :end1 2)))

;; sbcl handling of quasiquotes is different than cmucl:
;; https://github.com/thephoeron/let-over-lambda/blob/master/let-over-lambda.lisp
(defmacro defmacro/g! (name args &rest body)
  (let ((syms (remove-duplicates
               (remove-if-not #'g!-symbol-p
                              (flatten body)))))
    (multiple-value-bind (body declarations docstring)
        (alexandria:parse-body body :documentation t)
      `(defmacro ,name ,args
         ,@(when docstring
             (list docstring))
         ,@declarations
         (let ,(mapcar
                (lambda (s)
                  `(,s (gensym ,(subseq
                                 (symbol-name s)
                                 2))))
                syms)
           ,@body)))))

;; o! macrology
(defun o!-symbol-p (s)
  (and (symbolp s)
	   (> (length (symbol-name s)) 2)
	   (string= (symbol-name s)
				"O!" :start1 0 :end1 2)))

(defun o!-symbol-to-g!-symbol (s)
  (symb "G!"
        (subseq (symbol-name s) 2)))

(defmacro defmacro! (name args &rest body)
  (let* ((os (remove-if-not #'o!-symbol-p args))
         (gs (mapcar #'o!-symbol-to-g!-symbol os)))
    `(defmacro/g! ,name ,args
                  `(let ,(mapcar #'list (list ,@gs) (list ,@os))
                     ,(progn ,@body)))))


(defun get-nums (str)
  "Given str, if all are digits then return the integer, otherwise nil"
  (unless (zerop (length str))
    (if (every (lambda (c) (and (alphanumericp c)
                                (not (alpha-char-p c))))
               str)
        (parse-integer str)
      nil)))


(defun get-new-item-name (sequence base-name)
  "Return an item name that doesn't exist in sequence, using base-name as a base,
and appending a unique number to it if needed.

e.g. (get-new-item-name '(a b c) c) will return C1
     (get-new-item-name '(a b a1) a) will return A2
     (get-new-item-name '(a32 b a a33) a) will return A34

also see examples in test get-new-item-name-cases in utils-test.lisp
"
  
  (flet ((is-at-the-beginning (search-for search-in)
           (let ((pos (search search-for search-in :test 'equal)))
             (and pos (= 0 pos)))))
  
    (let* ((base-name (string base-name))
           (name-len (length base-name))
           (smallest-available 1)
           (new-name base-name))

      (labels ((increment-new-name-and-return-old (new-item-number)
                 (progn
                   (and (> new-item-number smallest-available)
                        (setf smallest-available (1+ new-item-number)))
                   (setf new-name (mkstr base-name smallest-available))
                   (incf smallest-available)
                   new-name))

               (get-item-number (item)
                 (or (parse-integer (subseq item name-len) :junk-allowed t)
                     0))

               (get-number-part-and-increment (new-item-number)
                 (parse-integer (subseq 
                                 (increment-new-name-and-return-old new-item-number)
                                 name-len) :junk-allowed t)))

      (loop for item in sequence
            do
            (when (is-at-the-beginning new-name (string item))
              (let ((item-number (get-item-number (string item))))
                (get-number-part-and-increment item-number))))
      new-name))))


(defun npush (obj place)
  "Push obj to the beginning of place, without creating new place
at the beginning, thus making references to the place aware of the push."
  (unless (typep place 'cons)
    (concatenate 'string "npush: You should pass a cons for place for npush to work."
                 "If you want to start with an empty cons, then pass (NIL)."))
  (if (equal place (cons nil nil))
      (setf (car place) obj)
    (progn
      (rplacd place (cons (first place) (rest place)))
      (rplaca place obj))))

(defun npush2 (obj place)
  (setf (cdr place) (cons (first place) (rest place)))
  (setf (car place) obj)
  place)

(defun current-symbols ()
  "Return the symbols in current package"
  (let ((list nil))
    (do-all-symbols (s (nreverse list))
      (when (eq (symbol-package s) *package*)
        (push s list)))))

(defun print-list (list)
  "Prints each item in the list at a separate line."
  (format t "~{~s~%~}" list))

(defmacro with-gensyms (syms &body body)
  "Make it easier to create gensyms for macros. Example:
A macro that creates a small amount of symbols before its actual
transformation:

(defmacro with-redraw ((var objs) &body body)
  (let ((gob (gensym))
        (x0 (gensym)) (y0 (gensym))
        (x1 (gensym)) (y1 (gensym)))
    ...))

now can be:
(defmacro with-redraw ((var objs) &body body)
  (with-gensyms (gob x0 y0 x1 y1)
    ...))"
  `(let ,(mapcar (lambda (s) `(,s (gensym)))
                 syms)
     ,@body))


(defun prune (test tree)
  "removes elements from a tree which are satisfying test"
  (labels ((rec (tree acc)
             (cond ((null tree) (nreverse acc))
                   ((consp (car tree))
                    (rec (cdr tree)
                         (cons (rec (car tree) nil) acc)))
                   (t (rec (cdr tree)
                           (if (funcall test (car tree))
                               acc
                             (cons (car tree) acc)))))))
    (rec tree nil)))

(unless (equal (prune #'evenp '(1 2 (3 (4 5) 6) 7 8 (9)))
               '(1 (3 (5)) 7 (9)))
  (error "fn:prune test1 failed."))


(defun before (x y lst &key (test #'eql))
  "Returns the cdr of lst beginning with the object given as the first argument,
given that it appears before the second argument."
  (and lst
       (let ((first (car lst)))
         (cond ((funcall test y first) nil)
               ((funcall test x first) lst)
               (t (before x y (cdr lst) :test test))))))


(defun after (x y lst &key (test #'eql))
  (let ((rest (before y x lst :test test)))
    (and rest (member x rest :test test))))

;; test
(unless (equal (after 'a 'b '(1 2 b a c))
			   '(a c))
  (error "fn:after test failed"))


(defun duplicate (obj lst &key (test #'eql))
  "Return rest of the lst beginningn with second obj,
if obj is duplicated in lst."
  (member obj (cdr (member obj lst :test test))
          :test test))

(unless (equal (duplicate 'a '(a b c a d))
               '(a d))
  (error "fn:duplicate test failed"))


(defun split-if (fn lst)
  "Split the lst at fn and return the split list as
two values."
  (let ((acc nil))
    (do ((src lst (cdr src)))
        ((or (null src) (funcall fn (car src)))
         (values (nreverse acc) src))
      (push (car src) acc))))

(unless (equal
         (multiple-value-list
          (split-if (lambda (x) (> x 4))
                    '(1 2 3 4 5 6 7 8 9 10)))
         '((1 2 3 4) (5 6 7 8 9 10)))
  (error "fn:split-if test failed"))


(defun most.test1 ()
  (most #'length '((a b) (a b c) (a) (e f g))))


(defun most.2 (fn lst &optional (test #'<))
  "Returns the biggest object and its fn call value."
  (if (null lst)
      (values nil nil)
    (let* ((wins (car lst))
           (max (funcall fn wins)))
      (dolist (obj (cdr lst))
        (let ((score (funcall fn obj)))
          (when (funcall test score max)
            (setq wins obj
                  max score))))
      (values wins max))))

(defun most2.test1 ()
  (unless
      (equal
       (multiple-value-list
        (most.2 #'length '((a b) (a b c) (a) (e f g)) #'>=))
       '((e f g) 3))
    (error "fn:most.2 failed first case.")))


(defun best (fn lst)
  (if (null lst)
      nil
    (let ((wins (car lst)))
      (dolist (obj (cdr lst))
        (if (funcall fn obj wins)
            (setq wins obj)))
      wins)))

(unless
    (equal	(best #'> '(1 2 3 4 5))
                5)
  (error "fn:best first test failed."))


(defun mostn (fn lst)
  "Return a list of all the elements for which fn yields
the highest score, along with the score itself."
  (if (null lst)
      (values nil nil)
    (let ((result (list (car lst)))
          (max (funcall fn (car lst))))
      (dolist (obj (cdr lst))
        (let ((score (funcall fn obj)))
          (cond ((> score max)
                 (setq max 	score
                       result (list obj)))
                ((= score max)
                 (push obj result)))))
      (values (nreverse result) max))))


(unless (equal
         (multiple-value-list
          (mostn #'length '((a b) (a b c) (a) (e f g))))
         '(((a b c) (e f g)) 3))
  (error "fn:mostn first test failed."))


(defun lrec (rec &optional base)
  "Returns a list recursion function which calls rec with first 
element and recurse with the caller's function.
This lrec should be able to generate most functions that recurse 
on successive cdrs of a list.

\"Note that this approach is not tail recursive and should be used
for prototyping. And a better solution can be found\" - Paul Graham.

The first argument to lrec must be a function of two arguments:
  the current car of the list, and a function which can be called 
  to continue the recursion."
  (labels ((self (lst)
             (if (null lst)
                 (if (functionp base)
                     (funcall base)
                   base)
               (funcall rec (car lst)
                        #'(lambda ()
                            (self (cdr lst)))))))
    #'self))

;; some usages of lrec above
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; list-length
(defvar my-length
  (lrec (lambda (x f)
		  (declare (ignore x))
		  (1+ (funcall f)))
		0))

;; copy-list
(defvar my-copy-list
  (lrec #'(lambda (x f) (cons x (funcall f)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;


;; (defun bestn (costfn iterfn data)
;;   "Iterate the data and find the best n objects, returning
;; them as a list."
;;   (labels ((local-iter ()
;; 			 ;; put iterfn and data to closure here
;; 			 nil-doesntworkyet))
;; 	(do ((obj (local-iter iterfn data) (setf obj (local-iter))))
;; 		 nil
;; 		  (values result cost))))

(defvar graph-1 '("1" ("1.1, 10" "1.2, 11" ("1.2.1, 20" "1.2.2, 12"))
                  "2" ("2.1" ("2.1.1, 30" "2.1.2" "2.1.3, 50")
                       "2.2" ("2.2.1") "2.3, 50")
                  ("3.1, 19" "3.2, 50" "3.3, 22")))


(defun memoize (fn)
  (let ((cache (make-hash-table :test #'equal)))
    (lambda (&rest args)
      (multiple-value-bind (val win) (gethash args cache)
        (if win
            val
          (setf (gethash args cache)
                (apply fn args)))))))

;; mapping
(defconstant +initial-array-size+ 10 
  "initial size for return array for mapa-b function")

(defun mapa-b (fn a b &optional (step 1))
  "map fn from a to b with increments of step"
  (do ((i a (+ i step))
       (result (make-array +initial-array-size+ :adjustable t :fill-pointer 0)))
      ((> i b) result)
    (vector-push (funcall fn i) result)))

(defun map0-n (fn n)
  "map fn from 0 to n"
  (mapa-b fn 0 n))

(defun map1-n (fn n)
  "map fn from 1 to n"
  (mapa-b fn 1 n))

(defun map-> (fn start test-fn succ-fn)
  "map fn to elements from start to every successor element."
  (do ((i start (funcall succ-fn i))
       (result (make-array +initial-array-size+ :adjustable t :fill-pointer 0)))
      ((funcall test-fn i) result)
    (vector-push (funcall fn i) result)))

(defun mappend (fn &rest lsts)
  "apply elements of lsts to fn and form another list with results."
  (apply #'append (apply #'mapcar fn lsts)))

(defun mapcars (fn &rest lsts)
  "apply fn to each element of each list in lsts"
  (let ((result (make-array +initial-array-size+ :adjustable t :fill-pointer 0)))
    (dolist (lst lsts)
      (dolist (obj lst)
        (vector-push (funcall fn obj) result)))
    result))

(defun rmapcar (fn &rest args)
  "mapcar for trees"
  (if (some #'atom args)
      (apply fn args)
    (apply #'mapcar
           #'(lambda (&rest args)
               (apply #'rmapcar fn args))
           args)))

;; anaphoric macros
(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
	 (if it ,then-form ,else-form)))

(defmacro awhen (test-form &body body)
  `(aif ,test-form
        (progn ,@body)))

(defmacro awhile (expr &body body)
  `(do ((it ,expr ,expr))
       ((not it))
     ,@body))

;; (awhile (poll *fridge*)
;;   (eat it))

(defmacro aand (&rest args)
  "During the evaluation of each of its arguments,
-it- will be bound to the value returned by the previous argument."
  (cond ((null args) t)				   ; return t as and if no arg
        ((null (cdr args)) (car args))	; terminate when there's one arg left
        (t `(aif ,(car args) (aand ,@(cdr args))))))

;; (aand (owner x) (address it) (town it))
;; compare above line to an alternative:
#|
(let ((own (owner x)))
  (if own
      (let ((adr (address own)))
        (if adr (town adr)))))
|#

(defmacro acond (&rest clauses)
  "This is meant for those cases where the remainder of a
cond clause wants to use the value returned by the test expression."
  (if (null clauses)
      nil
    (let ((cl1 (car clauses))
          (sym (gensym)))
      `(let ((,sym ,(car cl1)))
         (if ,sym
             (let ((it ,sym)) ,@(cdr cl1))
           (acond ,@(cdr clauses)))))))

(defmacro alambda (params &body body)
  "This is for referring literally to recursive functions."
  `(labels ((self ,params ,@body))
     #'self))

;; Factorial in anaphoric form:
;; (alambda (x) (if (= x 0) 1 (* x (self (1- x)))))
(defun fact (x)
  (if (= x 0)
      1
    (* x (fact (1- x)))))

(defmacro aif2 (test &optional then else)
  "the idiom used by gethash (return val & whether found multiple-values)
can be used by an aif2 macro, binding the first, and testing the second."
  (let ((win (gensym)))
    `(multiple-value-bind (it ,win) ,test
       (if (or it ,win) ,then ,else))))

;; similarly:
(defmacro awhen2 (test &body body)
  `(aif2 ,test
         (progn ,@body)))

(defmacro awhile2 (test &body body)
  (let ((flag (gensym)))
    `(let ((,flag t))
       (while ,flag
              (aif2 ,test
                    (progn ,@body)
                    (setq ,flag nil))))))

(defmacro acond2 (&rest clauses)
  (if (null clauses)
      nil
    (let ((cl1 (car clauses))
          (val (gensym))
          (win (gensym)))
      `(multiple-value-bind (,val ,win) ,(car cl1)
         (if (or ,val ,win)
             (let ((it ,val)) ,@(cdr cl1))
           (acond2 ,@(cdr clauses)))))))


(let ((g (gensym)))
  (defun read2 (&optional (str *standard-input*))
    (let ((val (read str nil g)))
      (unless (equal val g) (values val t)))))

(defmacro do-file (filename &body body)
  "usage example:
 (defun our-load (filename)
   (do-file filename (eval it)))"
  (let ((str (gensym)))
    `(with-open-file (,str ,filename)
       (awhile2 (read2 ,str)
         ,@body))))

;; above definition, so that we can say:
;; (defun our-load (filename)
;;   (do-file filename (eval it)))


(proclaim '(inline mklist))
(defun mklist (obj)
  (if (listp obj) obj (list obj)))


(defmacro for ((var start stop) &body body)
  "for loop, incrementing var by 1.
usage:

(for (i 0 3)
  (princ i))

prints: 0123
"
  (let ((gstop (gensym)))
    `(do ((,var ,start (1+ ,var))
          (,gstop ,stop))
         ((> ,var ,gstop))
       ,@body)))


(deflexical cont-lexical #'values)

(defmacro =lambda (parms &body body)
  `#'(lambda (cont-lexical ,@parms)
       ,@body))

(defmacro =defun (name parms &body body)
  (let ((f (intern (concatenate 'string
                                "=" (symbol-name name)))))
    `(progn
       (defmacro ,name ,parms
         `(,',f cont-lexical ,,@parms))
       (defun ,f (cont-lexical ,@parms)
         ,@body))))

(defmacro =bind (parms expr &body body)
  `(let ((cont-lexical (lambda ,parms ,@body))) ,expr))

(defmacro =values (&rest retvals)
  `(funcall cont-lexical ,@retvals))

(defmacro =funcall (fn &rest args)
  `(funcall ,fn cont-lexical ,@args))

(defmacro =apply (fn &rest args)
  `(apply ,fn cont-lexical ,@args))

(defmacro pull (obj place &rest args)
  (multiple-value-bind (vars forms var set access)
      (get-setf-expansion place)
    (let ((g (gensym)))
      `(let* ((,g ,obj)
              ,@(mapcar #'list vars forms)
              (,(car var) (delete ,g ,access ,@args)))
         ,set))))

(defmacro defun-in-package ((pkg from-fn to-fn) &body call-pattern)
    "Define a function named to-fn in package pkg, with the body call-pattern,
copying the documentation of from-fn over to to-fn.

Call-pattern must be a lambda expression.

e.g. call:
(defun-in-package (\"LIB\"
                   'lib~:delete-system-aux
                   \"DELETE-THIS-SYSTEM\")
  (lambda ()
    (lib~:delete-system-aux)))

will create a lib:delete-this-system function with the given lambda definition
and the documentation of 'lib~:delete-system-aux.

Why did I use this macro? Because I couldn't find a way to define this function in
lib (or std) package during compile-toplevel time, as I am creating lib package during
compilation at a different lexical scope - I think.
"
    (let ((pkg-name (gensym))
          (to-fn-sym (gensym)))
      `(let* ((,pkg-name (find-package ,pkg))
              (,to-fn-sym (intern ,to-fn ,pkg-name)))
         (setf (symbol-function ,to-fn-sym)
               ,@call-pattern

               (documentation ,to-fn-sym 'function)
               (documentation ,from-fn 'function)))))

(defmacro with-system ((sys-var sys-name) &body body)
  (let ((name (gensym)))
    `(let* ((,name ,sys-name)
            (,sys-var (gethash ,name *system-table*)))
       (if ,sys-var
           (progn
             ,@body)
         (error "System name ~a not found in *system-table*, consider adding
it in known-libs.lisp?~%"
                ,name)))))

(defmacro when-none-of ((&rest clauses) &body body)
  `(unless (or ,@clauses)
     (progn
       ,@body)))

#|
e.g.
(when-none-of ((= 0 1)
               (< 1 1))
  (print "none of them true"))
|#

(defun string-append (&rest strings)
  "This is faster than mkstr function, especially when compiled (about 2x), measured with LW7.1"
  (apply #'concatenate 'string strings))

(defmacro append-string (&rest strings)
  `(string-append ,@strings))

(defun print-object-to-string (obj)
  (with-output-to-string (s) 
    (print-object obj s)
    s))

(defun string-remove-escapes (str)
  "Remove the \\ and \" chars from str and return the result."
  (remove-if
   (lambda (c) (or (char-equal c #\\)
                   (char-equal c #\")))
   str))

(defmacro match-aref (ind &body scanner-code)
  "Run ppcre scan call scanner-code, and from the reg results choose ind item
"
  (let ((match (gensym))
        (regs (gensym)))
    `(multiple-value-bind (,match ,regs)
         ,@scanner-code
       (aref ,regs ,ind))))

