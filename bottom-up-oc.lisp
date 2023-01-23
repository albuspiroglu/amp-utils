;; mylib/utils/bottom-up-oc.lisp
;;;;
;;;; Utilities to help with bottom-up development & maintain o&c (observability
;;;; and controllability) of the abstractions that are bottom up.
;;;;


(in-package #:amp-dev)


(defun add-data-to-function (fn-name &optional file-path data-name)
  "Add a test file if it doesn't exist already.
Then add a with-fn-data-name macro, and add a test-fn-data-name.
If data-name is nil, they are replaced with next sequential number"
  (let* ((test-file-path (get-test-file-path fn-name
                                             (deduce-and-update-file-path fn-name)))

         (fn-tests       (get-fn-tests       test-file-path
                                             fn-name))

         (with-macro-fns (get-fn-with-macros test-file-path 
                                             fn-name)))

    (add-fn-test    test-file-path
                    (get-next-name fn-tests data-name))

    (add-with-macro test-file-path
                    (get-next-name with-macro-fns data-name))))


(defun deduce-and-update-file-path (fn-name)
  (let* ((file-path        (deduce-filepath fn-name))
         (symbol-file-path (get fn-name :file-path)))

    (if symbol-file-path
        (unless (equal symbol-file-path (get fn-name :file-path))
          (error "file-path of ~a, which is:~a and the given or deduced file-path:~a don't match."
                 fn-name symbol-file-path file-path))
      (setf (file-path-from-plist fn-name) file-path))

    file-path))

(defun get-test-file-path (fn-name file-path)
  "Return the corresponding test file for a file-path or the file that
the function fn-name resides."
  (make-pathname :directory (pathname-directory file-path)
                 :defaults (format nil "~A-test.lisp" (pathname-name file-path))))

(defun deduce-filepath (fn-name)
  "Try a few ways to find the file that has the function. Those ways are:
* file path is kept in plist of symbol fn-name
* file path is in description of symbol fn-name
* function is found in one of the lisp dev search paths
"
  (let ((file-path (file-path-from-plist fn-name)))
    (when file-path
      (return-from deduce-filepath file-path))

    (when (setf file-path (get-file-path-from-description fn-name))
      (return-from deduce-filepath file-path))

    (when (setf file-path (get-file-path-from-search-paths fn-name))
      (return-from deduce-filepath file-path))

    (error "filepath cannot be deduced for function ~a~%" fn-name)))

(defun file-path-from-plist (fn-name)
  (get fn-name :file-path))

(defun (setf file-path-from-plist) (value fn-name)
  (setf (get fn-name :file-path) value))

(defun get-file-path-from-description (fn-sym)
  "fn-sym: a symbol"
  (get-file-path-from-string 
   (string-remove-escapes
    (with-output-to-string (*standard-output*)
      (describe fn-sym)))))


(defun get-file-path-from-string (str)
  (let ((search-str "Source file: ")
        (after-space-till-end (ppcre:create-scanner "\\s*(.*)$" :multi-line-mode t)))
    (let ((start (search search-str str :test 'equalp)))
      (if start
          (parse-namestring
           (match-aref 0
             (ppcre:scan-to-strings
              after-space-till-end
              (subseq str (+ start (length search-str))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; let's write a function that'll read lisp files in a folder and if there are package definitions,
;; then parse the external symbols from that package and create an interface wrapper code
;; with some helpers.
#|
I decided with this approach when I wanted to add an opengl layer for garnet library. Why? Isn't it
already working with xlib? It actually is. I paused the effort before progressing much, because I
should put my effort in code visualisers before anything else.

But then, above here, I am working on bottom-up development helpers. Because code visualisers aren't
the only way to have more control on some code. You also need multiple pieces working together.

They are probably equally important. Seeing the structure of the code helps with understanding the parts
and architecture. But having tools for bottom up dev helps with progressing.

Since this file is named bottom-up-oc.lisp, and since visualisation of a code isn't exactly bottom-up,
perhaps I should separate the package definition for amp-dev and have another file, perhaps top-down-dev.lisp.

|#

(defun define-wrappers (folder)
  "Create a wrapper for the packages in folder, and save in folder/../folder_wrapperN/ (unique).
Wrapper has:
a packages.lisp including same package definitions, and a file for
each package (as packagename.lisp), including all external symbol definitions with
empty bodies, followed by a comment section with a copy of the same symbol definition from
folder, and a destination to the file and line number.
 
Steps:
Create an empty tree
Parse all the .lisp files recursive in folder,
for each package definition expression + export expressions:
add to tree:
  name of the package,
  a list of exported symbols

Then create the files with contents.
"
  (let ((wrapper-dir (get-wrapper-path folder))
        (tree nil))
    (amp-pathnames:walk-directory (lambda (f)
                                    (add-to-wrapper-tree f tree))
                                  folder)
    (dump-wrapper-tree tree wrapper-dir)))

(defun define-wrappers.test1 ()
  (ensure-directories-exist "/tmp/clx/")
  (define-wrappers "/tmp/clx/")
  (probe-file "/tmp/clx1"))

  
(defun get-wrapper-path (folder-name)
  "Given a folder name, return a non-existing folder name in:
folder/../folder-wrapper(N)
N is appended to get a unique name if folder-wrapper exists"
  (let* ((dirs (pathname-directory folder-name))
         (last-name (car (last dirs)))
         (parent-dir (butlast dirs))
         (new-name (amp-pathnames:get-unique-path parent-dir last-name)))
    (make-pathname :directory
                   (append (butlast dirs)
                           (cons new-name nil)))))

