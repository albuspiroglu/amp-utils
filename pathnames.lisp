;; mylib/utils/pathnames.lisp

;;;; chapter 15 practical: a portable pathname library

(in-package #:amp-pathnames)

(defun component-presentp (value)
  "will test whether a given component of a pathname is \"present,\"
meaning neither NIL nor the special value :unspecific"
  (and value (not (eql value :unspecific))))

(defun directory-pathnamep (p)
  "tests whether a pathname is already in directory form"
  (and
   (not (component-presentp (pathname-name p)))
   (not (component-presentp (pathname-type p)))
   p))

(defun pathname-as-directory (name)
  "converts any pathname to a directory form pathname"
  (let ((pathname (pathname name)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (if (not (directory-pathnamep name))
        (make-pathname
         :directory (append (or (pathname-directory pathname) (list :relative))
                            (list (file-namestring pathname)))
         :name nil
         :type nil
         :defaults pathname)
      pathname)))

(defun pathname-as-directory-2 (name)
  "converts any pathname to a directory form pathname"
  (let ((pathname (pathname name)))
    (if (not (directory-pathnamep name))
	(make-pathname
	 :directory (append (pathname-directory pathname))
	 :name nil
	 :type nil
	 :defaults pathname)
      pathname)))

(defun directory-wildcard (dirname)
  "CLISP has a quirk for filetype wild so correct it"
  (make-pathname
   :name :wild
   :type #-clisp :wild #+clisp nil
   :defaults (pathname-as-directory dirname)))

(defun list-directory (dirname)
  (when (wild-pathname-p dirname)
    (error "Can only list concrete directory names."))
  (let ((wildcard (directory-wildcard dirname)))
    
    #+(or sbcl cmu lispworks)
    (directory wildcard)

    #+openmcl
    (directory wildcard :directories t)

    #+allegro
    (directory wildcard :directories-are-files nil)

    #+clisp
    (nconc
     (directory wildcard)
     (directory (clisp-subdirectories-wildcard wildcard)))

    #-(or sbcl cmu lispworks openmcl allegro clisp)
    (error "list-directory not implemented")))

#+clisp
(defun clisp-subdirectories-wildcard (wildcard)
  (make-pathname
   :directory (append (pathname-directory wildcard) (list :wild))
   :name nil
   :type nil
   :defaults wildcard))

(defun file-existsp (pathname)
  #+(or sbcl lispworks openmcl)
  (probe-file pathname)

  #+(or allegro cmu)
  (or (probe-file (pathname-as-directory pathname))
      (probe-file pathname))

  #+clisp
  (or (ignore-errors
        (probe-file (pathname-as-file pathname)))
      (ignore-errors
        (let ((directory-form (pathname-as-directory pathname)))
          (when (ext:probe-directory directory-form)
            directory-form))))

  #-(or sbcl cmu lispworks openmcl allegro clisp)
  (error "file-existsp not implemented"))

(defun pathname-as-file (name)
  (let ((pathname (pathname name)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (if (directory-pathnamep name)
        (let* ((directory (pathname-directory pathname))
               (name-and-type (pathname (first (last directory)))))
          (make-pathname
           :directory (butlast directory)
           :name (pathname-name name-and-type)
           :type (pathname-type name-and-type)
           :defaults pathname))
      pathname)))

(defun walk-directory (dirname fn &key
                               directories
                               (itemtest (constantly t))
                               (dirtest (constantly t)))
  "Takes the name of a directory and a function and call the function
on the pathnames of all the files under the directory, recursively.
When :directories is true, it will call the function on the pathnames
of directories as well as regular files.
The :itemtest argument, if provided, specifies another function that's invoked
on each pathname before the main function is; the main function will be called 
only if the test function returns true.
If :dirtest is true, then walk to that dir, otherwise skip it."
  (labels
      ((walk (name)
         (cond
          ((directory-pathnamep name)
           (when (and directories (funcall itemtest name))
             (funcall fn name))
           (dolist (x (list-directory name))
             (when (funcall dirtest x)
               (walk x))))
          ((funcall itemtest name) (funcall fn name)))))
    (walk (pathname-as-directory dirname))))

;; this is to rename multiple files (most probably using wildcards)
(defun rename-files-this-doesnt-work (from to)
  (dolist (file (list-directory (pathname-as-directory-2 from)))
    (rename-file file (translate-pathname file from to))))

;; (setq files (list-directory (pathname-as-directory-2 "./*.bin")))

;; code below directly from hyperspec
(defun rename-files (from to)
   (dolist (file (directory from))
     (rename-file file (translate-pathname file from to))))

;; some examples
;; (rename-files "*.bin" "*.lisp")

(defun append-paths (path1 &rest other-paths)
  "Given some paths, join them together to a pathname. All items but the last one must end
with a directory char, i.e. /
Also, all items except path1 must be relative paths. Path1 can be either absolute or relative.

E.g. either of the calls below:
      (append-paths \"dir1/\" \"dir2/\" \"file.txt\")
      (append-paths \"dir1/\" \"dir2/file.txt\")

will return:
#P\"dir1/dir2/file.txt\"
"
  (let ((last-path (car (last other-paths))))
    (make-pathname :directory
                   (append
                    (pathname-directory path1)
                    (mappend (lambda (p) (remove :relative (pathname-directory p)))
                             other-paths))
                   :name (pathname-name last-path)
                   :type (pathname-type last-path))))

(defmacro join-paths (path1 &rest other-paths)
  `(append-paths ,path1 ,@other-paths))

(defun get-unique-path (parent-dir local-dir)
  "Return the string parent-dir/local-dir if it doesn't exist. Or return a
string parent-dir/local-dir(N), incrementing the number until a unique name
is found."
  (get-new-item-name (dir-items-as-strings parent-dir)
                     local-dir))

(defun dir-items-as-strings (parent-dir)
  (mapcan (lambda (item) (last (pathname-directory item)))
          (mapcar #'namestring
                  (directory (make-pathname :directory
                                            (pathname-directory parent-dir) 
                                            :name :wild)))))
